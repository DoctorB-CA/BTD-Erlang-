# FSM Architecture Diagrams - BTD-Erlang Game Objects

> **Note:** To view these diagrams as images instead of code, paste the mermaid code into:
> - [Mermaid Live Editor](https://mermaid.live) 
> - GitHub (when viewing .md files)
> - VS Code with Mermaid extension
> - Or any Mermaid-compatible viewer

---

## 1. Monkey FSM Architecture

```mermaid
stateDiagram-v2
    direction LR
    [*] --> searching
    searching --> attacking : Target found<br/>Fire arrow
    attacking --> searching : Cooldown complete
    
    state searching {
        direction LR
        scan --> found : find_bloon {ok, BloonId}
        scan --> no_target : find_bloon {error, not_found}
        found --> fire_arrow : spawn arrow FSM
        no_target --> scan : 300ms timeout
        fire_arrow --> [*]
    }
    
    state attacking {
        direction LR
        cooldown --> ready : 800ms timeout
        ready --> [*]
    }
```

**Monkey Data:** `{type, pos, range, region_pid}` | **Scan:** 300ms | **Cooldown:** 800ms

---

## 2. Bloon (Balloon) FSM Architecture

```mermaid
stateDiagram-v2
    direction LR
    [*] --> moving
    moving --> hit_check : {hit, Damage}
    hit_check --> moving : Health > 0
    hit_check --> [*] : Health ≤ 0<br/>Send banana reward
    moving --> [*] : End of path<br/>Game over
    
    state moving {
        direction LR
        move_tick --> path_check : 50ms interval
        path_check --> end_path : NextPos undefined
        path_check --> region_check : NextPos valid
        region_check --> same_region : No boundary cross
        region_check --> migrate : Cross region boundary
        same_region --> move_tick : Update position
        migrate --> spawn_new : Cross-node migration
        migrate --> local_update : Same-node migration
        spawn_new --> [*] : Terminate this process
        local_update --> move_tick : Update region
        end_path --> [*] : Notify game over
    }
    
    state hit_check {
        direction LR
        damage --> survive : Health > 0
        damage --> die : Health ≤ 0
        survive --> [*]
        die --> [*]
    }
```

**Bloon Data:** `{id, index, health, pos, current_region_pid, region_pids, region_id}` | **Move:** 50ms | **Migration:** Cross-node support

---

## 3. Arrow (Dart) FSM Architecture

```mermaid
stateDiagram-v2
    direction LR
    [*] --> flying
    flying --> [*] : Hit target OR Out of range OR Target lost
    
    state flying {
        direction LR
        move_tick --> range_check : 50ms interval
        range_check --> out_of_range : steps_left ≤ 0
        range_check --> target_check : steps_left > 0
        target_check --> target_lost : No target position
        target_check --> calculate : Target found
        calculate --> hit_check : Move toward target
        hit_check --> hit_target : Distance ≤ 20px
        hit_check --> continue : Distance > 20px
        hit_target --> [*] : Send damage to bloon
        continue --> move_tick : Decrement steps
        target_lost --> [*] : Clean up
        out_of_range --> [*] : Clean up
    }
```

**Arrow Data:** `{id, type, pos, target_id, region_id, steps_left}` | **Speed:** 10px/tick | **Range:** 300px | **Hit:** 20px threshold

---

# Gen_Server Architecture Diagrams

## 4. GUI Gen_Server Architecture

```mermaid
stateDiagram-v2
    direction LR
    [*] --> idle
    idle --> processing_cast : handle_cast
    idle --> processing_call : handle_call
    idle --> processing_event : handle_info (wx events)
    
    processing_cast --> idle : Game state updates
    processing_call --> idle : Synchronous requests
    processing_event --> idle : User interactions
    
    state processing_cast {
        direction LR
        game_updates --> monkey_ops : add/delete/move monkey
        game_updates --> balloon_ops : add/delete/move balloon  
        game_updates --> dart_ops : add/delete/move dart
        game_updates --> ui_updates : change_bananas, refresh
        game_updates --> game_events : lose_game, clear_board
        game_updates --> batch_updates : update_balloons/darts
        
        monkey_ops --> [*] : Update monkey map
        balloon_ops --> [*] : Update balloon map
        dart_ops --> [*] : Update dart map
        ui_updates --> [*] : Update widgets
        game_events --> [*] : Show dialogs
        batch_updates --> [*] : Refresh display
    }
    
    state processing_event {
        direction LR
        wx_paint --> render : Draw all objects
        wx_mouse --> place_monkey : Left click placement
        wx_button --> button_action : Monkey selection/wave start
        wx_erase --> clear_bg : Background clearing
        
        render --> [*] : Paint complete
        place_monkey --> [*] : Send to main_server
        button_action --> [*] : UI state change
        clear_bg --> [*] : Prevent flicker
    }
```

**GUI State:** `{frame, board, bitmaps, monkeys, balloons, darts, placing, banana_text_widget, button_ids}` 

**Key Operations:** Rendering (paint), User input (mouse/buttons), Game state display, wxErlang event handling

## 5. Main_Server Gen_Server Architecture

```mermaid
stateDiagram-v2
    direction LR
    [*] --> ready
    ready --> processing_cast : handle_cast
    ready --> processing_call : handle_call  
    ready --> processing_timer : handle_info (timers)
    
    processing_cast --> ready : Command processing
    processing_call --> ready : Synchronous queries
    processing_timer --> ready : Periodic updates
    
    state processing_cast {
        direction LR
        game_flow --> game_over_handler : game_over events
        game_flow --> restart_handler : restart_game
        game_flow --> spawn_commands : add_monkey/bloon
        
        economy --> banana_rewards : balloon_destroyed
        economy --> cost_validation : place_item
        
        game_over_handler --> [*] : Call GUI lose_game
        restart_handler --> [*] : Clear DB, notify regions
        spawn_commands --> [*] : Route to regions
        banana_rewards --> [*] : Update banana count
        cost_validation --> [*] : Check/deduct cost
    }
    
    state processing_timer {
        direction LR
        gui_update --> db_query : Get all bloons/darts
        db_query --> batch_send : Send to GUI
        batch_send --> [*] : 33ms interval (30 FPS)
    }
```

**Main_Server State:** `{region_pids, game_over, bananas}` | **Economy:** Banana costs/rewards | **Coordination:** Cross-region management

## 6. Region_Server Gen_Server Architecture

```mermaid
stateDiagram-v2
    direction LR
    [*] --> managing
    managing --> processing_call : handle_call
    managing --> processing_cast : handle_cast
    
    processing_call --> managing : Queries
    processing_cast --> managing : Commands
    
    state processing_call {
        direction LR
        queries --> find_bloon : find_bloon request
        queries --> ping : Health check
        
        find_bloon --> db_scan : Query multiple regions
        db_scan --> distance_calc : Find closest in range
        distance_calc --> result : Return BloonId or not_found
        ping --> pong : Return self
        result --> [*] : Complete
        pong --> [*] : Complete
    }
    
    state processing_cast {
        direction LR
        spawning --> spawn_monkey : Create monkey FSM
        spawning --> spawn_bloon : Create bloon FSM
        spawning --> spawn_migration : Handle bloon migration
        
        game_events --> balloon_end : balloon_reached_end
        game_events --> balloon_destroyed : banana rewards
        game_events --> restart_cleanup : Process cleanup
        
        spawn_monkey --> monkey_created : monkey start_link
        spawn_bloon --> bloon_created : bloon start_link  
        spawn_migration --> migration_done : Cross-node spawning
        balloon_end --> notified : Notify main_server
        balloon_destroyed --> forwarded : Forward to main_server
        restart_cleanup --> cleaned : Kill game processes
        
        monkey_created --> [*] : Complete
        bloon_created --> [*] : Complete
        migration_done --> [*] : Complete
        notified --> [*] : Complete
        forwarded --> [*] : Complete
        cleaned --> [*] : Complete
    }
```

**Region_Server State:** `{id, total_regions}` | **Responsibilities:** Local game object management, Cross-region queries, Process spawning

---

## FSM Communication Overview

```mermaid
flowchart LR
    M[Monkey FSM] -->|find_bloon| R[Region Server]
    R -->|{ok, BloonId}| M
    M -->|spawn| A[Arrow FSM]
    A -->|{hit, Damage}| B[Bloon FSM]
    B -->|banana_reward| R
    R -->|banana_reward| MS[Main Server]
    MS -->|update| GUI[GUI]
    
    subgraph "Database"
        DB[(Mnesia)]
    end
    
    M -.->|write_monkey| DB
    B -.->|write/delete_bloon| DB
    A -.->|write/delete_dart| DB
    R -.->|read queries| DB
```

## Gen_Server Communication & Message Flow

```mermaid
flowchart LR
    subgraph "User Interaction"
        User[Player] -->|Click/Button| GUI
    end
    
    subgraph "Main Node"
        GUI[GUI Server] 
        MS[Main Server]
    end
    
    subgraph "Worker Nodes"
        R1[Region 0]
        R2[Region 1] 
        R3[Region 2]
        R4[Region 3]
    end
    
    subgraph "Game Objects"
        M[Monkey FSMs]
        B[Bloon FSMs]
        A[Arrow FSMs]
    end
    
    %% User to GUI
    GUI -->|place_item| MS
    GUI <--|change_bananas| MS
    GUI <--|lose_game| MS
    
    %% Main Server to Regions
    MS -->|spawn_monkey/bloon| R1
    MS -->|spawn_monkey/bloon| R2
    MS -->|spawn_monkey/bloon| R3
    MS -->|spawn_monkey/bloon| R4
    MS -->|restart_cleanup| R1
    MS -->|restart_cleanup| R2
    MS -->|restart_cleanup| R3
    MS -->|restart_cleanup| R4
    
    %% Regions to Main Server  
    R1 -->|game_over/banana_reward| MS
    R2 -->|game_over/banana_reward| MS
    R3 -->|game_over/banana_reward| MS
    R4 -->|game_over/banana_reward| MS
    
    %% Regions spawn Game Objects
    R1 --> M
    R1 --> B
    R2 --> M
    R2 --> B
    R3 --> M
    R3 --> B
    R4 --> M
    R4 --> B
    
    %% Game Object Interactions
    M -->|find_bloon| R1
    M -->|spawn arrow| A
    A -->|hit damage| B
    B -->|destruction reward| R1
    
    %% GUI Updates
    MS -->|update_balloons/darts| GUI
    MS -.->|33ms timer| MS
```

## Message Types & Frequencies

| **Server** | **Message Type** | **Frequency** | **Source/Target** | **Purpose** |
|------------|------------------|---------------|-------------------|-------------|
| **GUI** | `{add_monkey, T,X,Y,I}` | On placement | User → GUI | Add monkey to display |
| **GUI** | `{update_balloons, Map}` | 30 FPS | Main → GUI | Batch balloon updates |
| **GUI** | `{change_bananas, Amount}` | On economy change | Main → GUI | Update banana display |
| **GUI** | `#wx{event=#wxPaint{}}` | On refresh | wxErlang → GUI | Redraw game board |
| **Main** | `{place_item, {MT,X,Y}}` | On placement | GUI → Main | Validate & spawn monkey |
| **Main** | `{balloon_destroyed, Id, Health}` | On death | Region → Main | Award bananas |
| **Main** | `{game_over, BloonId}` | On path end | Region → Main | Trigger lose condition |
| **Main** | `update_gui_balloons` | 33ms timer | Timer → Main | Periodic GUI updates |
| **Region** | `{find_bloon, Pos, Range}` | 300ms | Monkey → Region | Target acquisition |
| **Region** | `{spawn_monkey, Type, Pos, Range}` | On placement | Main → Region | Create monkey FSM |
| **Region** | `{spawn_bloon, Health, Pids, RegionId}` | On wave start | Main → Region | Create bloon FSM |

## State Management & Persistence

| **Server** | **State Persistence** | **Cleanup Strategy** | **Fault Tolerance** |
|------------|-----------------------|----------------------|---------------------|
| **GUI** | In-memory maps | Automatic on restart | Restart on crash |
| **Main** | Banana count, region PIDs | Database clear + process kill | Supervisor restart |
| **Region** | Region ID only | Process enumeration & kill | Supervisor restart |

**Database Role:** Central state store for all game objects, cross-node replication, ACID transactions
