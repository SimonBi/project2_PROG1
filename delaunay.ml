(*
Project 2
Course : PROG1
Authors : Simon Bihel
Institute : ENS Rennes, Computer Science Department
*)

open List

type point = {x: float; y: float};;
type triangle = {p1: point; p2: point; p3: point};;
type point_set = point list;;
type triangle_set = triangle list;;

let rec random nb max_x max_y = if nb = 0 then []
  else (Random.int max_x, Random.int max_y)::(random (nb-1) max_x max_y);;

let ccw a b c = 
  let m1 = (fst b) - (fst a)
  and m2 = (fst c) - (fst a)
  and m3 = (snd b) - (snd a)
  and m4 = (snd c) - (snd a)
  in let det = (m1*m4)-(m2*m3)
  in det >= 0;;

let direct t = 
  let a = hd t
  and b = hd (tl t)
  and c = hd (tl (tl t)) in
  if ccw a b c then [a; b; c]
  else if ccw a c b then [a; c; b]
  else if ccw b a c then [b; a; c]
  else if ccw b c a then [b; c; a]
  else if ccw c a b then [c; a; b]
  else [c; b; a];;

let rm_col_row a col = 
  let rec rm_row a' i = if i = 0 then tl a'
                        else (hd a')::(rm_row (tl a') (i-1)) in
  let rec rm_col a' i = if length a' = 0 then []
                        else (rm_row (hd a') i)::(rm_col (tl a') i) in
  let m_list = Array.to_list (Array.map Array.to_list a) in
  let newA = rm_row (rm_col m_list 0) col in
  Array.of_list (map Array.of_list newA);;

let determinant m =
  let rec determinant' indexes =
    let get_point i j = m.(fst indexes.(i).(j)).(snd indexes.(i).(j)) in
    if Array.length indexes = 2 then
      let a' = get_point 0 0
      and b' = get_point 0 1
      and c' = get_point 1 0
      and d' = get_point 1 1 in
      (a' * d') - (b' * c')
    else
      let rec decomp_det i =
        if i = Array.length indexes then 0
        else ((-1 * (i mod 2)) * (get_point 0 i)
              * (determinant' (rm_col_row indexes i)))
             + (decomp_det (i+1)) in
      decomp_det 0
  in
  determinant' (Array.init (Array.length m)
                (fun x -> (Array.init (Array.length m) (fun y -> (x,y)))));;

let in_circle t d = 
  let correctT = direct t in
  let a = hd correctT
  and b = hd (tl correctT)
  and c = hd (tl (tl correctT)) in
  let m = [|[|fst a; snd a; ((fst a) * (fst a)) + ((snd a) * (snd a)); 1|]; 
            [|fst b; snd b; ((fst b) * (fst b)) + ((snd b) * (snd b)); 1|]; 
            [|fst c; snd c; ((fst c) * (fst c)) + ((snd c) * (snd c)); 1|]; 
            [|fst d; snd d; ((fst d) * (fst d)) + ((snd d) * (snd d)); 1|]|] in
  determinant m >= 0;;

print_int (determinant [|[|-1; 2; 5|]; [|1; 2; 3|]; [|-2; 8; 10|]|]);;
