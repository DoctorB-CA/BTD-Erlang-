# FSM Architecture Diagrams - BTD-Erlang Game Objects

## 1. Monkey FSM Architecture

```mermaid
stateDiagram-v2
    [*] --> searching : start_link/5<br/>Init monkey data<br/>Write to DB<br/>Set scan timeout
    
    state searching {
        [*] --> scanning : state_timeout(0, scan)
        scanning --> found_target : find_bloon returns {ok, BloonId}
        scanning --> no_target : find_bloon returns {error, not_found}
        no_target --> scanning : state_timeout(300ms, scan)
        found_target --> [*] : fire_arrow()<br/>spawn arrow FSM
    }
    
    searching --> attacking : Target found<br/>Arrow fired
    
    state attacking {
        [*] --> cooling_down : ATTACK_COOLDOWN(800ms)
        cooling_down --> [*] : cooldown_over
    }
    
    attacking --> searching : Cooldown complete<br/>Resume scanning
    
    note right of searching
        **Scanning Process:**
        - Call region_server:find_bloon()
        - Check range (distance calculation)
        - Return closest bloon within range
        - Scan interval: 300ms
    end note
    
    note right of attacking
        **Attack Process:**
        - Determine dart type from monkey type
        - Avatar monkey: random dart type
        - Spawn arrow:start_link()
        - Cooldown: 800ms
    end note
```

### Monkey FSM Details

**States:**
- **searching**: Default state, continuously scans for targets
- **attacking**: Cooldown period after firing an arrow

**Data Structure:**
```erlang
-record(data, {type, pos, range, region_pid}).
```

**Key Features:**
- **Target Acquisition**: Uses region server to find bloons in range
- **Dart Type Mapping**: Different monkey types shoot different darts
- **Avatar Special**: Shoots random dart types
- **Performance**: 300ms scan interval, 800ms attack cooldown

---

## 2. Bloon (Balloon) FSM Architecture

```mermaid
stateDiagram-v2
    [*] --> init_check : start_link/3 or<br/>start_link_migration/6
    
    state init_check {
        [*] --> new_bloon : start_link (new)
        [*] --> migrated_bloon : start_link_migration
        new_bloon --> [*] : Create path<br/>Set start position<br/>Write to DB
        migrated_bloon --> [*] : Update position<br/>Recalc region<br/>Write to DB
    }
    
    init_check --> moving : Init complete<br/>Set move timeout
    
    state moving {
        [*] --> path_check : state_timeout(50ms, move)
        path_check --> end_reached : NextPos == undefined
        path_check --> continue_moving : NextPos valid
        
        continue_moving --> region_check : Update position<br/>Write to DB
        region_check --> same_region : No region change
        region_check --> migrate_needed : Cross region boundary
        
        same_region --> [*] : Continue in region<br/>Set next timeout
        migrate_needed --> spawn_migration : Cross-node migration
        migrate_needed --> local_region : Same-node migration
        
        spawn_migration --> [*] : Spawn on new node<br/>Terminate this process
        local_region --> [*] : Update region<br/>Continue moving
        
        end_reached --> [*] : Notify region<br/>Game over signal
    }
    
    moving --> hit_processing : cast/info {hit, Damage}
    
    state hit_processing {
        [*] --> damage_calc : Reduce health<br/>Update DB
        damage_calc --> destroyed : Health <= 0
        damage_calc --> continue : Health > 0
        destroyed --> [*] : Send banana reward<br/>Delete from DB
        continue --> [*] : Return to moving
    }
    
    hit_processing --> moving : Health > 0
    hit_processing --> [*] : Health <= 0<br/>Balloon destroyed
    
    moving --> [*] : End of path reached<br/>Game over
    
    note right of moving
        **Movement System:**
        - 50ms intervals (20 FPS)
        - Path index increments
        - Region boundaries at 200px
        - Cross-node migration support
        - Path ends at {799, 600}
    end note
    
    note right of hit_processing
        **Combat System:**
        - Accept hit from arrows
        - Health reduction
        - Banana reward calculation
        - Database cleanup on death
    end note
```

### Bloon FSM Details

**States:**
- **moving**: Primary state, handles movement and migration
- **hit_processing**: Temporary state for damage calculation

**Data Structure:**
```erlang
-record(state, {id, index, health, pos, current_region_pid, region_pids, region_id}).
```

**Key Features:**
- **Path Movement**: Follows predefined path with 50ms intervals
- **Cross-Node Migration**: Seamless migration between worker nodes
- **Health System**: Takes damage, awards bananas on destruction
- **Global Registration**: Discoverable across distributed system

---

## 3. Arrow (Dart) FSM Architecture

```mermaid
stateDiagram-v2
    [*] --> flying : start_link/4<br/>Create dart record<br/>Write to DB<br/>Set max range
    
    state flying {
        [*] --> range_check : state_timeout(0, move)
        range_check --> out_of_range : steps_left <= 0
        range_check --> target_check : steps_left > 0
        
        target_check --> target_lost : get_target_position fails
        target_check --> calculate_movement : Target position found
        
        calculate_movement --> hit_check : Move toward target<br/>Update DB position
        hit_check --> hit_target : Distance <= HIT_THRESHOLD(20px)
        hit_check --> continue_flying : Distance > HIT_THRESHOLD
        
        hit_target --> [*] : Send damage to bloon<br/>Delete from DB
        continue_flying --> [*] : Decrement steps<br/>Set next timeout
        target_lost --> [*] : Delete from DB
        out_of_range --> [*] : Delete from DB
    }
    
    flying --> [*] : Hit target OR<br/>Out of range OR<br/>Target lost
    
    note right of flying
        **Flight System:**
        - Speed: 10 pixels per tick
        - Tick rate: 50ms
        - Max range: 300 pixels
        - Hit threshold: 20 pixels
        - Vector calculation to target
    end note
    
    note left of flying
        **Targeting System:**
        - Track target by BloonId
        - Get position from database
        - Calculate movement vector
        - Handle target destruction
    end note
```

### Arrow FSM Details

**States:**
- **flying**: Only state, handles movement and collision detection

**Data Structure:**
```erlang
-record(data, {id, type, pos, target_id, region_id, steps_left}).
```

**Key Features:**
- **Projectile Physics**: Vector-based movement toward target
- **Range Limitation**: Maximum 300 pixels (30 steps at 10px/step)
- **Hit Detection**: 20 pixel threshold for collision
- **Target Tracking**: Follows moving bloons using database lookups

---

## FSM Communication Patterns

```mermaid
sequenceDiagram
    participant M as Monkey FSM
    participant R as Region Server
    participant B as Bloon FSM
    participant A as Arrow FSM
    participant DB as Mnesia DB
    
    Note over M: Searching State
    M->>R: find_bloon(pos, range)
    R->>DB: get_bloons_in_regions()
    DB-->>R: [BloonRecords]
    R-->>M: {ok, BloonId}
    
    Note over M: Attack State
    M->>A: arrow:start_link(type, pos, target)
    A->>DB: write_dart(DartRecord)
    
    Note over A: Flying State
    loop Every 50ms
        A->>DB: get_target_position(BloonId)
        DB-->>A: {ok, TargetPos}
        A->>A: calculate_vector() & move_towards()
        A->>DB: write_dart(UpdatedRecord)
        
        alt Hit Target
            A->>B: gen_statem:cast({hit, Damage})
            A->>DB: delete_dart(DartId)
            Note over B: Hit Processing
            B->>DB: write_bloon(UpdatedHealth)
            alt Bloon Destroyed
                B->>R: balloon_destroyed message
                R->>Main: banana reward
                B->>DB: delete_bloon(BloonId)
            end
        else Out of Range
            A->>DB: delete_dart(DartId)
        end
    end
```

## Performance Characteristics

| FSM Type | State Duration | Update Frequency | DB Operations |
|----------|---------------|------------------|---------------|
| **Monkey** | searching: continuous<br/>attacking: 800ms | scan: 300ms<br/>attack: on target | write_monkey (init)<br/>No updates during operation |
| **Bloon** | moving: continuous | movement: 50ms | write_bloon (every move)<br/>delete_bloon (on death) |
| **Arrow** | flying: ~1.5s avg | movement: 50ms | write_dart (every move)<br/>delete_dart (on hit/expire) |

## Memory & Process Management

- **Process Lifecycle**: All FSMs are temporary processes
- **Cleanup**: Automatic on process termination
- **Global Registry**: Only bloons are globally registered
- **Local Registry**: Monkeys and arrows are not registered
- **Supervision**: No direct supervision (fail-fast design)
- **Database Consistency**: ACID transactions ensure state consistency
