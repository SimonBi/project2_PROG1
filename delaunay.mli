open List

type point = {x : float; y : float}
type triangle = {p1 : point; p2 : point; p3 : point}
type point_set = point list
type triangle_set = triangle list

val random: int -> int -> int -> point list

val ccw: point -> point -> point -> bool

val in_circle: triangle -> point -> bool

val border: triangle_set -> (point * point) list

val add_point: triangle_set -> point -> triangle_set

val delaunay: point_set -> int -> int -> triangle_set

val draw_points: point_set -> unit

val draw_triangles: triangle_set -> unit

val delaunay_step_by_step: point_set -> unit
