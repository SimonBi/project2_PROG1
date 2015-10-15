open List

type point = {x : float; y : float}
type triangle = {p1 : point; p2 : point; p3 : point}
type point_set = point list
type triangle_set = triangle list

val random: int -> int -> int -> point list

val ccw : point -> point -> point -> bool

val in_circle : triangle -> point -> bool

val border : triangle_set -> (point * point) list

val add_point : triangle_set -> point -> triangle_set
