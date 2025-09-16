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
flowchart TD
    subgraph "Game Objects Communication"
        M[Monkey FSM] -->|find_bloon request| R[Region Server]
        R -->|BloonId response| M
        M -->|spawn arrow| A[Arrow FSM]
        A -->|hit with damage| B[Bloon FSM]
        B -->|banana_reward| R
        R -->|forward reward| MS[Main Server]
        MS -->|banana update| GUI[GUI]
    end
    
    subgraph "Database Operations"
        DB[(Mnesia Database)]
        M -.->|write monkey| DB
        B -.->|write/delete bloon| DB
        A -.->|write/delete dart| DB
        R -.->|read queries| DB
    end
    
    subgraph "Cross-Region Communication"
        R -->|find_bloon query| R2[Other Regions]
        R2 -->|bloon data| R
        B -->|migration| R3[Target Region]
        R3 -->|spawn migrated bloon| B2[New Bloon FSM]
    end
```

## Gen_Server Communication & Message Flow

```mermaid
flowchart TD
    subgraph "User Layer"
        User[Player] -->|Click/Button Events| GUI[GUI Server]
    end
    
    subgraph "Main Node"
        GUI
        MS[Main Server]
    end
    
    subgraph "Worker Nodes"
        R1[Region Server 0]
        R2[Region Server 1] 
        R3[Region Server 2]
        R4[Region Server 3]
    end
    
    subgraph "Game Objects Layer"
        M[Monkey FSMs]
        B[Bloon FSMs]
        A[Arrow FSMs]
    end
    
    subgraph "Database Layer"
        DB[(Mnesia Database)]
    end
    
    %% User to Main System
    GUI -->|place_item requests| MS
    MS -->|banana updates| GUI
    MS -->|lose_game signal| GUI
    
    %% Main Server to Regions
    MS -->|spawn_monkey commands| R1
    MS -->|spawn_monkey commands| R2
    MS -->|spawn_bloon commands| R3
    MS -->|spawn_bloon commands| R4
    MS -->|restart_cleanup| R1
    MS -->|restart_cleanup| R2
    MS -->|restart_cleanup| R3
    MS -->|restart_cleanup| R4
    
    %% Regions to Main Server  
    R1 -->|game_over events| MS
    R2 -->|banana_reward events| MS
    R3 -->|game_over events| MS
    R4 -->|banana_reward events| MS
    
    %% Regions spawn Game Objects
    R1 --> M
    R1 --> B
    R2 --> M
    R2 --> B
    R3 --> A
    R4 --> A
    
    %% Game Object Interactions
    M -->|find_bloon queries| R1
    M -->|spawn_arrow commands| A
    A -->|hit_damage messages| B
    B -->|destruction_reward| R1
    
    %% GUI Updates
    MS -->|update_balloons batch| GUI
    MS -->|update_darts batch| GUI
    
    %% Database Operations
    M -.->|read/write| DB
    B -.->|read/write| DB
    A -.->|read/write| DB
    R1 -.->|queries| DB
    R2 -.->|queries| DB
    R3 -.->|queries| DB
    R4 -.->|queries| DB
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

---

# Mnesia Database Architecture

## 7. Mnesia Distributed Database System

```mermaid
flowchart TD
    subgraph "Main Node - btd@localhost"
        MainSchema[(Schema<br/>Disc Copies)]
        MainBloon[(Bloon Table<br/>Disc Copies)]
        MainMonkey[(Monkey Table<br/>Disc Copies)]
        MainDart[(Dart Table<br/>Disc Copies)]
        MainMnesia[Mnesia Process<br/>Main Node]
    end
    
    subgraph "Worker Node 1 - worker1@localhost"
        W1Schema[(Schema<br/>RAM Copy)]
        W1Bloon[(Bloon Table<br/>RAM Copy)]
        W1Monkey[(Monkey Table<br/>RAM Copy)]
        W1Dart[(Dart Table<br/>RAM Copy)]
        W1Mnesia[Mnesia Process<br/>Worker 1]
    end
    
    subgraph "Worker Node 2 - worker2@localhost"
        W2Schema[(Schema<br/>RAM Copy)]
        W2Bloon[(Bloon Table<br/>RAM Copy)]
        W2Monkey[(Monkey Table<br/>RAM Copy)]
        W2Dart[(Dart Table<br/>RAM Copy)]
        W2Mnesia[Mnesia Process<br/>Worker 2]
    end
    
    subgraph "Worker Node 3 - worker3@localhost"
        W3Schema[(Schema<br/>RAM Copy)]
        W3Bloon[(Bloon Table<br/>RAM Copy)]
        W3Monkey[(Monkey Table<br/>RAM Copy)]
        W3Dart[(Dart Table<br/>RAM Copy)]
        W3Mnesia[Mnesia Process<br/>Worker 3]
    end
    
    subgraph "Worker Node 4 - worker4@localhost"
        W4Schema[(Schema<br/>RAM Copy)]
        W4Bloon[(Bloon Table<br/>RAM Copy)]
        W4Monkey[(Monkey Table<br/>RAM Copy)]
        W4Dart[(Dart Table<br/>RAM Copy)]
        W4Mnesia[Mnesia Process<br/>Worker 4]
    end
    
    %% Schema Distribution
    MainSchema -.->|Replicate Schema| W1Schema
    MainSchema -.->|Replicate Schema| W2Schema
    MainSchema -.->|Replicate Schema| W3Schema
    MainSchema -.->|Replicate Schema| W4Schema
    
    %% Table Replication (Disc to RAM)
    MainBloon -.->|Real-time Sync| W1Bloon
    MainBloon -.->|Real-time Sync| W2Bloon
    MainBloon -.->|Real-time Sync| W3Bloon
    MainBloon -.->|Real-time Sync| W4Bloon
    
    MainMonkey -.->|Real-time Sync| W1Monkey
    MainMonkey -.->|Real-time Sync| W2Monkey
    MainMonkey -.->|Real-time Sync| W3Monkey
    MainMonkey -.->|Real-time Sync| W4Monkey
    
    MainDart -.->|Real-time Sync| W1Dart
    MainDart -.->|Real-time Sync| W2Dart
    MainDart -.->|Real-time Sync| W3Dart
    MainDart -.->|Real-time Sync| W4Dart
    
    %% Write Operations (Workers to Main)
    W1Mnesia -->|Write Operations| MainMnesia
    W2Mnesia -->|Write Operations| MainMnesia
    W3Mnesia -->|Write Operations| MainMnesia
    W4Mnesia -->|Write Operations| MainMnesia
    
    %% Transaction Coordination
    MainMnesia -->|Transaction Commit| W1Mnesia
    MainMnesia -->|Transaction Commit| W2Mnesia
    MainMnesia -->|Transaction Commit| W3Mnesia
    MainMnesia -->|Transaction Commit| W4Mnesia
```

## 8. Mnesia Table Structures & Records

```mermaid
erDiagram
    BLOON {
        atom id PK "Unique bloon identifier"
        integer health "Current health points"
        integer index "Position on path (0-N)"
        tuple pos "Coordinates {X, Y}"
        integer region_id "Owner region (0-3)"
    }
    
    MONKEY {
        atom id PK "Unique monkey identifier"
        atom type "ground|water|fire|air|avatar"
        tuple pos "Coordinates {X, Y}"
        integer range "Attack range in pixels"
        integer region_id "Owner region (0-3)"
    }
    
    DART {
        atom id PK "Unique dart identifier"
        atom type "ground|water|fire|air|avatar"
        tuple pos "Coordinates {X, Y}"
        atom target_id FK "Target bloon ID"
        integer region_id "Owner region (0-3)"
    }
    
    BLOON ||--o{ DART : "targeted_by"
    MONKEY ||--o{ DART : "shoots"
```

## 9. Mnesia Transaction Flow & ACID Properties

```mermaid
flowchart TD
    subgraph "Transaction Lifecycle"
        Start[Transaction Start] --> Lock[Acquire Locks]
        Lock --> Read[Read Operations]
        Read --> Write[Write Operations]
        Write --> Validate[Validate Consistency]
        Validate --> Commit[Two-Phase Commit]
        Commit --> Release[Release Locks]
        Release --> Complete[Transaction Complete]
        
        Validate -->|Conflict| Abort[Abort Transaction]
        Abort --> Rollback[Rollback Changes]
        Rollback --> Release
    end
    
    subgraph "ACID Properties Implementation"
        A[Atomicity<br/>All-or-nothing commits<br/>Automatic rollback on failure]
        C[Consistency<br/>Schema validation<br/>Foreign key constraints]
        I[Isolation<br/>Lock-based concurrency<br/>Snapshot isolation]
        D[Durability<br/>Disc copies on main node<br/>Write-ahead logging]
    end
    
    subgraph "Distribution Strategy"
        MainDisc[Main Node<br/>Disc Copies<br/>Persistent Storage]
        WorkerRAM[Worker Nodes<br/>RAM Copies<br/>Fast Read Access]
        
        MainDisc -->|Replicate| WorkerRAM
        WorkerRAM -->|Write Through| MainDisc
    end
```

## 10. Database Operations & Performance

| **Operation** | **Table** | **Type** | **Frequency** | **Performance** | **Node Distribution** |
|---------------|-----------|----------|---------------|-----------------|----------------------|
| **write_bloon** | bloon | INSERT/UPDATE | High (per bloon move) | 1-2ms | All nodes (disc+RAM) |
| **delete_bloon** | bloon | DELETE | Medium (on death) | 1ms | All nodes (disc+RAM) |
| **write_monkey** | monkey | INSERT | Low (on placement) | 1ms | All nodes (disc+RAM) |
| **write_dart** | dart | INSERT | High (per shot) | 1-2ms | All nodes (disc+RAM) |
| **delete_dart** | dart | DELETE | High (on hit/miss) | 1ms | All nodes (disc+RAM) |
| **get_all_bloons** | bloon | SELECT ALL | 30 FPS (GUI) | 2-5ms | Local RAM (fast) |
| **get_all_darts** | dart | SELECT ALL | 30 FPS (GUI) | 2-5ms | Local RAM (fast) |
| **find_bloon_query** | bloon | SELECT WHERE | 300ms (targeting) | 1-3ms | Local RAM (fast) |
| **db_clear** | ALL | TRUNCATE | On restart | 10-50ms | Main node (disc) |

## 11. Mnesia Clustering & Fault Tolerance

```mermaid
flowchart LR
    subgraph "Normal Operation"
        MainActive[Main Node<br/>ACTIVE<br/>Disc Storage]
        W1Active[Worker 1<br/>ACTIVE<br/>RAM Copy]
        W2Active[Worker 2<br/>ACTIVE<br/>RAM Copy]
        W3Active[Worker 3<br/>ACTIVE<br/>RAM Copy]
        W4Active[Worker 4<br/>ACTIVE<br/>RAM Copy]
        
        MainActive -.->|Sync| W1Active
        MainActive -.->|Sync| W2Active
        MainActive -.->|Sync| W3Active
        MainActive -.->|Sync| W4Active
    end
    
    subgraph "Failure Scenarios"
        MainDown[Main Node<br/>DOWN<br/>Data Loss Risk]
        W1Down[Worker 1<br/>DOWN<br/>No Impact]
        W2Running[Worker 2<br/>RUNNING<br/>Cached Data]
        W3Running[Worker 3<br/>RUNNING<br/>Cached Data]
        W4Running[Worker 4<br/>RUNNING<br/>Cached Data]
        
        MainDown -.->|No Sync| W2Running
        MainDown -.->|No Sync| W3Running
        MainDown -.->|No Sync| W4Running
    end
    
    subgraph "Recovery Strategy"
        Restart[Restart Game<br/>Fresh Database]
        ClearTables[db_clear Operation<br/>Reset All Tables]
        Respawn[Respawn All Objects<br/>Clean State]
        
        Restart --> ClearTables
        ClearTables --> Respawn
    end
```

## 12. Database Access Patterns by Component

```mermaid
flowchart TD
    subgraph "Database Access Patterns"
        
        subgraph "Monkey FSM Access"
            MonkeyFSM[Monkey FSM] -->|write_monkey on spawn| MonkeyWrite[Monkey Table]
            MonkeyFSM -->|delete_monkey on death| MonkeyDelete[Monkey Table]
        end
        
        subgraph "Bloon FSM Access"
            BloonFSM[Bloon FSM] -->|write_bloon on move| BloonWrite[Bloon Table]
            BloonFSM -->|write_bloon on damage| BloonUpdate[Bloon Table]
            BloonFSM -->|delete_bloon on death| BloonDelete[Bloon Table]
        end
        
        subgraph "Arrow FSM Access"
            ArrowFSM[Arrow FSM] -->|write_dart on spawn| DartWrite[Dart Table]
            ArrowFSM -->|write_dart on move| DartUpdate[Dart Table]
            ArrowFSM -->|delete_dart on hit| DartDelete[Dart Table]
        end
        
        subgraph "Region Server Access"
            RegionServer[Region Server] -->|get_bloons_in_regions| BloonQuery[Bloon Table Query]
            RegionServer -->|find_bloon targeting| BloonSearch[Bloon Table Search]
        end
        
        subgraph "Main Server Access"
            MainServer[Main Server] -->|get_all_bloons for GUI| BloonRead[Bloon Table Read]
            MainServer -->|get_all_darts for GUI| DartRead[Dart Table Read]
            MainServer -->|db_clear on restart| DBClear[All Tables Clear]
        end
        
        subgraph "GUI Access"
            GUI[GUI Server] -->|Receives batch updates| GUIUpdate[No Direct DB Access]
        end
    end
```

**Storage Distribution:** Main node = Persistent disc storage | Worker nodes = Fast RAM cache  
**Consistency Model:** Strong consistency with 2-phase commit | Real-time replication  
**Performance:** 1-5ms operations | 30 FPS GUI updates | Cross-node query support
