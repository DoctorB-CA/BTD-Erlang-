-record(bloon, {
    id,         % Unique ID for the bloon (primary key)
    type,       % The type of the bloon (e.g., red_bloon, blue_bloon)
    health,     % Current health
    path,       % The full path the bloon follows
    path_index, % The current step on the path
    region_id   % The ID of the region the bloon is in
}).

-record(monkey, {
    id,         % Unique ID for the monkey (primary key)
    pos,        % The {X, Y} position of the monkey
    range,      % The attack range of the monkey
    region_id,  % The ID of the region the monkey is in
    type        % The type of the monkey
}).
