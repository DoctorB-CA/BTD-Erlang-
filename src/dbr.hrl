-record(bloon, {
    id,         % Unique ID for the bloon (primary key)
    health,     % Current health
    index,      % Current index on the path
    pos,        % Current position {X, Y}
    region_id   % The ID of the region the bloon is in
}).

-record(monkey, {
    id,         % Unique ID for the monkey (primary key)
    type,       % Type of monkey (e.g., ground_monkey)
    pos,        % The {X, Y} position of the monkey
    range,      % The attack range of the monkey
    region_id   % The ID of the region the monkey is in
}).

-record(dart, {
    id,         % Unique ID for the dart (primary key)
    type,       % Type of dart (e.g., ground_dart, fire_dart)
    pos,        % Current position {X, Y}
    target_id,  % ID of the target bloon
    region_id   % The ID of the region the dart is in
}).
