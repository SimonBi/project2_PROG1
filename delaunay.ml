(*
Project 2
Course : PROG1
Authors : Simon Bihel
Institute : ENS Rennes, Computer Science Department
*)

open List;;

type point = {x: float; y: float};;
type triangle = {p1: point; p2: point; p3: point};;
type point_set = point list;;
type triangle_set = triangle list;;

let print_array a = for i = 0 to ((Array.length a)-1) do
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
else (print_tuple (hd l); print_list_tuple (tl l));;

let rec random nb max_x max_y = if nb = 0 then []
else {x= (Random.float (float_of_int max_x)); y= (Random.float (float_of_int max_y))}
       ::(random (nb-1) max_x max_y);;

let ccw a b c = 
  let m1 = (b.x) -. (a.x)
  and m2 = (c.x) -. (a.x)
  and m3 = (b.y) -. (a.y)
  and m4 = (c.y) -. (a.y)
  in let det = (m1*.m4) -. (m2*.m3)
  in det >= 0.;;

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

let rm_col_row a col = 
  let rec rm_row a' i = if i = 0 then tl a'
                        else (hd a')::(rm_row (tl a') (i-1)) in
  let rec rm_col a' i = if length a' = 0 then []
                        else (rm_row (hd a') i)::(rm_col (tl a') i) in
  let m_list = Array.to_list (Array.map Array.to_list a) in
  let newA = rm_col (rm_row m_list 0) col in
  Array.of_list (map Array.of_list newA);;

let determinant m =
  let rec determinant' indexes =
    print_matrix indexes;
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

let in_circle t d = 
  let correctT = direct t in
  let a = correctT.p1
  and b = correctT.p2
  and c = correctT.p3 in
  let m = [|[|a.x; a.y; ((a.x)**2.) +. ((a.y)**2.); 1.|]; 
            [|b.x; b.y; ((b.x)**2.) +. ((b.y)**2.); 1.|]; 
            [|c.x; c.y; ((c.x)**2.) +. ((c.y)**2.); 1.|]; 
            [|d.x; d.y; ((d.x)**2.) +. ((d.y)**2.); 1.|]|] in
  determinant m >= 0.;;

let edges_triangle t = [(t.p1, t.p2); (t.p1, t.p3); (t.p2, t.p3)];;

let rec extract_edges t_set = 
  if length t_set = 0 then []
  else (edges_triangle (hd t_set)) @ (extract_edges (tl t_set));;

let rec keep_single l = 
  if length l = 0 then []
  else try ignore (find (fun x -> x=(hd l) || (snd x, fst x)=(hd l)) (tl l)); 
           keep_single (filter (fun x -> x<>(hd l) && (snd x, fst x)<>(hd l)) (tl l))
       with | Not_found -> (hd l)::(keep_single (tl l));;

let border t_set = 
  let edges_set = extract_edges t_set in
  keep_single edges_set;;

let rec select_t t_set p =
  if length t_set = 0 then []
  else if (in_circle (hd t_set) p) then
         (hd t_set)::(select_t (tl t_set) p)
       else select_t (tl t_set) p;;

let rec rm_t t_set p =
  if length t_set = 0 then []
  else if (in_circle (hd t_set) p) then
         rm_t (tl t_set) p
       else (hd t_set)::(rm_t (tl t_set) p);;

let rec new_triangles edges_set p =
  if length edges_set = 0 then []
  else begin
    let t = hd edges_set in
    {p1=t.p1; p2=t.p2; p3=p}::(new_triangles (tl edges_set) p)
  end;;

let add_point t_set p =
  let to_rm_t = select_t t_set p in
  (new_triangles to_rm_t p) @ (rm_t t_set p);;

(* print_list_tuple (border [[(1.,3.);(2.,0.);(4.,5.)];[(4.,5.);(7.,6.);(3.,9.)];[(4.,5.);(2.,0.);(7.,0.)]]);; *)
