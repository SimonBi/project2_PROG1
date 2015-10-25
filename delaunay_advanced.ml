(*
Project 2
Course : PROG1
Authors : Simon Bihel
Institute : ENS Rennes, Computer Science Department
*)
(* #load "graphics.cma"
#load "unix.cma" *)
open Graphics;;
open List;;

close_graph ();;
open_graph " 800x600";;
(* Random.init(int_of_float(Sys.time()));;*)
Random.self_init ();;

(** ########## Global variables that can be changed. ########## *)
let dim = (800, 600);;
let wait_time = 0.1;;
let nb_points = 350;;

type point = {x: float; y: float};;
type triangle = {p1: point; p2: point; p3: point};;
type point_set = point list;;
type triangle_set = triangle list;;

(*let print_array a = for i = 0 to ((Array.length a)-1) do
                      print_int (fst a.(i)); print_string ","; print_int (snd a.(i)); print_string " "
                    done; print_string "\n";;
let print_matrix m = for i = 0 to ((Array.length m)-1) do
                       print_array m.(i)
                     done;;
let print_tuple_int t = print_int (fst t); print_string ","; print_int (snd t); print_string " ";;
let print_tuple t = print_tuple_int (fst t); print_string ",,"; print_tuple_int (snd t);;
let rec print_list l = if length l = 0 then print_string "\n"
                       else (print_int (hd l); print_string " "; print_list (tl l));;
let rec print_list_tuple l = if length l = 0 then print_string "\n"
else (print_tuple (hd l); print_list_tuple (tl l));;*)

let print_point p = print_string "("; print_float p.x; print_string ","; print_float p.y; print_string ")";;

(** Pause the process. *)
let minisleep sec = 
  try ignore (Unix.select [] [] [] sec) with 
    | _ -> print_string "Sleep not working on OSX or interrupted system call.\n";;

(** Create a random point in a limited space. *)
let rec random nb max_x max_y = if nb = 0 then []
else {x= (Random.float (float_of_int max_x)); y= (Random.float (float_of_int max_y))}
       ::(random (nb-1) max_x max_y);;

(** Test if a triangle is in its direct form. *)
let ccw a b c = 
  let m1 = (b.x) -. (a.x)
  and m2 = (c.x) -. (a.x)
  and m3 = (b.y) -. (a.y)
  and m4 = (c.y) -. (a.y)
  in let det = (m1*.m4) -. (m2*.m3)
  in det >= 0.;;

(** Put a triangle in its direct form. *)
let direct t = 
  let a = t.p1
  and b = t.p2
  and c = t.p3 in
  if ccw a b c then {p1=a;p2=b;p3=c}
  else if ccw a c b then {p1=a;p2=c;p3=b}
  else if ccw b a c then {p1=b;p2=a;p3=c}
  else if ccw b c a then {p1=b;p2=c;p3=a}
  else if ccw c a b then {p1=c;p2=a;p3=b}
  else {p1=c;p2=b;p3=a};;

(** Remove the first row and a particular column of a matrix. *)
let rm_col_row a col = 
  let rec rm_row a' i = 
    if length a' = 0 then []
    else (
      if i = 0 then tl a'
      else (hd a')::(rm_row (tl a') (i-1)) ) in
  let rec rm_col a' i = if length a' = 0 then []
                        else (rm_row (hd a') i)::(rm_col (tl a') i) in
  let m_list = Array.to_list (Array.map Array.to_list a) in
  let newA = rm_col (rm_row m_list 0) col in
  Array.of_list (map Array.of_list newA);;

(** Compute the determinant of a nxn matrix. *)
let determinant m =
  let rec determinant' indexes =
    let get_point i j = m.(fst indexes.(i).(j)).(snd indexes.(i).(j)) in
    if Array.length indexes = 2 then
      let a' = get_point 0 0
      and b' = get_point 0 1
      and c' = get_point 1 0
      and d' = get_point 1 1 in
      (a' *. d') -. (b' *. c')
    else
      let rec decomp_det i =
        if i = Array.length indexes then 0.
        else ((-1. ** (float_of_int (i mod 2))) *. (get_point 0 i)
              *. (determinant' (rm_col_row indexes i)))
             +. (decomp_det (i+1)) in
      decomp_det 0
  in
  determinant' (Array.init (Array.length m)
                (fun x -> (Array.init (Array.length m) (fun y -> (x,y)))));;

let is_flat t =
  let s1x = t.p1.x -. t.p2.x
  and s1y = t.p1.y -. t.p2.y
  and s2x = t.p2.x -. t.p3.x
  and s2y = t.p2.y -. t.p3.y
  and s3x = t.p3.x -. t.p1.x
  and s3y = t.p3.y -. t.p1.y in
  (s2y-.s1y) *. (s3x-.s2x) = (s3y-.s2y) *. (s2x-.s1x);;

(** Test if a point is in the circumscribed circle of a triangle. *)
let in_circle t d =
  if is_flat t then false
  else
  let correctT = direct t in
  let a = correctT.p1
  and b = correctT.p2
  and c = correctT.p3 in
  let m = [|[|a.x; a.y; ((a.x)**2.) +. ((a.y)**2.); 1.|]; 
            [|b.x; b.y; ((b.x)**2.) +. ((b.y)**2.); 1.|]; 
            [|c.x; c.y; ((c.x)**2.) +. ((c.y)**2.); 1.|]; 
            [|d.x; d.y; ((d.x)**2.) +. ((d.y)**2.); 1.|]|] in
  determinant m >= 0.;;

(** Get the list of edges of a single triangle. *)
let edges_triangle t = [(t.p1, t.p2); (t.p1, t.p3); (t.p2, t.p3)];;

(** Get the list of edges of all the triangles of a list. *)
let rec extract_edges t_set = 
  if length t_set = 0 then []
  else (edges_triangle (hd t_set)) @ (extract_edges (tl t_set));;

let rec extract_points t_set =
  if length t_set = 0 then []
  else 
    let t = hd t_set in
    [t.p1; t.p2; t.p3] @ (extract_points (tl t_set));;

let rec remove l e =
  if length l = 0 then []
  else 
    if hd l = e then tl l
    else (hd l)::(remove (tl l) e);;

let rec remove_doubles l =
  if length l = 0 then []
  else (hd l)::(remove (tl l) (hd l));;

let rec rm_points_in_points points rm_points =
  if length rm_points = 0 then points
  else rm_points_in_points (remove points (hd rm_points)) (tl rm_points);;

(** Remove the items that appear more than one time in a list. *)
let rec keep_single l = 
  if length l = 0 then []
  else try ignore (find (fun x -> x=(hd l) || (snd x, fst x)=(hd l)) (tl l));
           keep_single (filter (fun x -> x<>(hd l) &&
                                        (snd x, fst x)<>(hd l)) (tl l))
       with | Not_found -> (hd l)::(keep_single (tl l));;

(** Get the convex border of a list of triangles. *)
let border t_set = 
  let edges_set = extract_edges t_set in
  keep_single edges_set;;

(** Remove from a list the triangles that don't have the point in their 
circumscribed circle. *)
let rec select_t t_set p =
  if length t_set = 0 then []
  else if (in_circle (hd t_set) p) then
         (hd t_set)::(select_t (tl t_set) p)
       else select_t (tl t_set) p;;

(** Remove from a list the triangles that have the point in their circumscribed
circle. *)
let rec rm_t t_set p =
  if length t_set = 0 then []
  else if (in_circle (hd t_set) p) then
         rm_t (tl t_set) p
       else (hd t_set)::(rm_t (tl t_set) p);;

(** From the list of edges that constitute the convex border, create the new 
triangles with the new point. *)
let rec new_triangles edges_set p =
  if length edges_set = 0 then []
  else begin
    let t = hd edges_set in
    {p1=(fst t); p2=(snd t); p3=p}::(new_triangles (tl edges_set) p)
  end;;

(** Add a new point to the existing triangles with eventual changes to them. *)
let add_point t_set p =
  let to_rm_t = select_t t_set p in
  (new_triangles (border to_rm_t) p) @ (rm_t t_set p);;

let edges_from_points p =
  let rec edges' p' =
    if length p' = 1 then []
    else (hd p', hd (tl p'))::(edges' (tl p')) in
  (hd p, nth p ((length p)-1))::(edges' p);;

let rec min_points p =
  if length p = 1 then hd p
  else let min_rest = min_points (tl p) in
  if (hd p).y < min_rest.y then hd p
  else min_rest;;

let angle v1 v2 =
  let v11 = (snd v1).x -. (fst v1).x
  and v12 = (snd v1).y -. (fst v1).y
  and v21 = (snd v2).x -. (fst v2).x
  and v22 = (snd v2).y -. (fst v2).y in
  let angle1 = atan2 v22 v21
  and angle2 = atan2 v12 v11 in
  let angleF = angle1 -. angle2 in
  if angleF > 0. then angleF -. 7.
  else angleF;;
  

let convex_hull points =
  let u = min_points points 
  and local_min = ref infinity in
  let points' = ref (remove points u)
  and precedents = ref [u] 
  and v = ref u in
  while length !points' > 0
  do
    let a = angle (u, {x=u.x+.1.;y=u.y}) (u, hd !points') in
    if a < !local_min then (
      local_min := a;
      v := hd !points' );
    points' := tl !points';
  done;
  precedents := !v::( !precedents);
  let points_save = ref points in
  while !v <> u
  do
    points_save := remove !points_save !v;
    points' := !points_save;
    let succ_v = ref u in
    local_min := infinity;
    let pred_v = hd (tl !precedents) in
    while length !points' > 0
    do
      let a' = angle ( !v, pred_v) ( !v, hd !points') in
      if a' < !local_min then (
          local_min := a';
          succ_v := hd !points' );
      points' := tl !points'
    done;
    precedents := !succ_v::( !precedents);
    v := !succ_v
  done;
  edges_from_points (tl !precedents);;

let init_delaunay points =
  let border_edges = convex_hull points in
  let border_points = map (fun x -> fst x) border_edges in
  new_triangles border_edges (hd (rm_points_in_points points border_points));;

let rm_points_in_t t_set points =
  let p2rm = ref (remove_doubles (extract_points t_set))
  and res = ref points in
  while length !p2rm > 0 do
    res := remove !res (hd !p2rm);
    p2rm := tl !p2rm
  done;
  !res;;

(** Do the Delaunay's triangulation on a list of points in a limited space. *)
let delaunay points = 
  let init_triangles = init_delaunay points in
  let remaining_points = rm_points_in_t init_triangles points in
  let rec insert_points points' t_set' = 
    if length points' = 0 then t_set'
    else add_point (insert_points (tl points') t_set') (hd points') in
  insert_points remaining_points init_triangles;;

(** Draw points of a list. *)
let rec draw_points points =
  set_color red;
  if length points > 0 then (
    let p = hd points in
    fill_circle (int_of_float p.x) (int_of_float p.y) 4;
    draw_points (tl points)
  )

(** Draw points that should be hidden. *)
let rec draw_hidden_points points =
  set_color black;
  if length points > 0 then (
    let p = hd points in
    fill_circle (int_of_float p.x) (int_of_float p.y) 4;
    draw_hidden_points (tl points)
  )

(** Draw the point that will be added next. *)
let draw_point_added point =
  set_color green;
  fill_circle (int_of_float point.x) (int_of_float point.y) 4;;

(** Draw a triangle and its points. *)
let draw_triangle t = 
  set_color black;
  draw_poly (Array.of_list 
              (map (fun x -> (int_of_float x.x, int_of_float x.y)) 
                   [t.p1; t.p2; t.p3]));
  draw_points [t.p1; t.p2; t.p3];;

(** Draw triangles of a list. *)
let rec draw_triangles triangles =
  if length triangles = 0 then ()
  else (draw_triangle (hd triangles); draw_triangles (tl triangles));;

(** Do the delaunay's triangulation with pauses and graphical visualization. *)
let delaunay_step_by_step_old points =
  let max_x = float_of_int (fst dim)
  and max_y = float_of_int (snd dim) in
  let t1 = {p1={x=0.;y=0.}; p2={x=0.;y=max_y}; p3={x=max_x;y=max_y}}
  and t2 = {p1={x=0.;y=0.}; p2={x=max_x;y=0.}; p3={x=max_x;y=max_y}} in
  let rec insert_points points' t_set' hidden_points' = 
    if length points' = 0 then t_set'
    else (
      let new_triangles = insert_points (tl points') t_set' ((hd points')::hidden_points') in
      minisleep wait_time;
      while button_down () do minisleep wait_time done;
      clear_graph ();
      draw_point_added (hd points');
      draw_hidden_points hidden_points';
      draw_triangles new_triangles;
      add_point new_triangles (hd points') ) in
  let final_triangles = insert_points points [t1; t2] [] in
  minisleep wait_time;
  clear_graph ();
  draw_triangles final_triangles;;

let delaunay_step_by_step points =
  let init_triangles = init_delaunay points in
  let remaining_points = rm_points_in_t init_triangles points in
  let rec insert_points points' t_set' hidden_points' = 
    if length points' = 0 then t_set'
    else (
      let new_triangles = insert_points (tl points') t_set' ((hd points')::hidden_points') in
      minisleep wait_time;
      while button_down () do minisleep wait_time done;
      clear_graph ();
      draw_point_added (hd points');
      draw_hidden_points hidden_points';
      draw_triangles new_triangles;
      add_point new_triangles (hd points') ) in
  let final_triangles = insert_points remaining_points init_triangles [] in
  minisleep wait_time;
  clear_graph ();
  draw_triangles final_triangles;;

delaunay_step_by_step (random nb_points (fst dim) (snd dim));;
(* draw_triangles (delaunay (random nb_points (fst dim) (snd dim)));;*)

(** Ask to exit. *)
let quit_loop = ref false in
while not !quit_loop
do
  print_string "Exit ? (y/n) ";
  let command_input = read_line () in
  if command_input.[0] = 'y' then
    quit_loop := true
done;;
