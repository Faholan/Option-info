(*
MIT License

Copyright (c) 2021 Faholan

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

(*Recherche dichotomique dans un tableau trié dans l'ordre croissant*)

let recherche m t =
  let rec aux i j =
    if i >= j then
      m = t.(i)
    else
      let k = (i+j)/2 in match m with
        | _ when t.(k) = m -> true
        | _ when t.(k) > m -> aux i (k-1)
        | _ -> aux (k+1) j
  in aux 0 (Array.length t - 1)
;;

(*Résolution approchée de f(x)=0 pour f monotone*)

let solve f a b eps =
  let rec aux u v =
    if v -. u < eps then
      u
    else
      if (f u) *. (f ((u +. v) /. 2.)) < 0. then
        aux u ((u +. v) /. 2.)
      else
        aux ((u +. v) /. 2.) v
  in aux a b
;;

(*Tri fusion*)

let rec decoupe = function
  | [] -> [], []
  | [a] -> [a], []
  | a::b::t -> let f, s = decoupe t in (a::f), (b::s)
;;

let rec fusion l1 l2 = match (l1, l2) with
  | ([], l) -> l
  | (l, []) -> l
  | (h::t, h'::t') when h < h' -> h::(fusion t l2)
  | (h::t, h'::t') -> h'::(fusion l1 t')
;;

let rec tri_fusion = function
  | [] -> []
  | [a] -> [a]
  | l -> let l1, l2 = decoupe l in
    fusion (tri_fusion l1) (tri_fusion l2)
;;

(*Quicksort*)

let rec separe x = function
  | [] -> [], []
  | h::t -> let l1, l2 = separe t in
    if h <= x then
      (h::l1), l2
    else
      l1, (h::l2)
;;

let rec quicksort = function
  | [] -> []
  | h::t -> let l1, l2 = separe h t in
    (quicksort l1)@(h::(quicksort l2))
;;

(*Quicksort - vecteurs*)

let echange i j t =
  let aux = t.(i) in
    t.(i) <- t.(j) ;
    t.(j) <- aux
;;

let separation t i j =
  let g = ref (i + 1) and d = ref j in
    while !d >= !g do
      if t.(!g) <= t.(i) then
        incr g
      else (
        echange !g !d t ;
        decr d
      )
    done ;
    if !d > i then
      echange i !d t ;
    !d
;;

let tri_rapide t =
  let rec aux i j =
    if i < j then
      let k = separation t i j in (
        aux i (k - 1) ;
        aux (k + 1) j
      )
  in aux 0 (Array.length t - 1)
;;
