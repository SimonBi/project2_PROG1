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

let determinant m = 
  if Array.length m = 2 then (m.(0).(0) * m.(1).(1)) - (m.(0).(1) * m.(1).(0))
  else Array.length m;;

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
