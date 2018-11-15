type ('k, 'v) t

val prazen : ('k, 'v) t
val dodaj : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
val poisci : 'k -> ('k, 'v) t -> 'v option
