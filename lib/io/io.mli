open Types

val parse_point : string -> point option
val format_point : point -> string
val read_points : unit -> point list
val read_point_stream : unit -> point option
