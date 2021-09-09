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

(*Problème 1*)

(*Question 1*)

(*a*)

let maxsomme arr =
  let cur = ref 0 and cur_max = ref arr.(0) in
    for i = 0 to Array.length arr - 1 do
      cur := 0 ;
      for j = i to Array.length arr - 1 do
        cur := !cur + arr.(j) ;
        cur_max := max !cur !cur_max
      done ;
    done ;
    !cur_max
;;

(*Question 2*)

(*a*)

let maxsomme2 arr =
  let rec aux i j = match j - i with
    | 0 -> arr.(i), arr.(i), arr.(i), arr.(i)
    | n -> let s1, msg1, ms1, msd1 = aux i (i + n/2) and s2, msg2, ms2, msd2 = aux (i + n/2 + 1) j in
      (s1 + s2), (max msg1 (s1 + msg2)), (max ms1 (max ms2 (msd1 + msg2))), (max msd2 (msd1 + s2))
  in let _, _, final, _ = aux 0 (Array.length arr - 1) in final
;;

(*Problème 2*)

(*Question 1*)

(*a*)

let occurences x t i j =
  let count = ref 0 in
    for k = i to j do
      if t.(k) = x then
        incr count
    done ;
    !count
;;

(*c*)

let majoritaire arr =
  let n = Array.length arr - 1 in
    let rec aux i =
      if i = n then
        false, 0
      else
        if occurences arr.(i) arr i n > n/2 then
          true, arr.(i)
        else
          aux (i+1)
    in aux 0
;;

(*Question 2*)

(*b*)

let majoritaire2 arr =
  let rec majo i j = match j - i with
    | 0 -> true, arr.(i)
    | n -> let b1, x1 = majo i (i + (n-1)/2) and b2, x2 = majo (i + (n+1)/2) j in
      match (b1, b2) with
      | (false, false) -> false, 0 (*Pas de majoritaire nécessairement*)
      | (true, false) -> let count = occurences x1 arr i j in
        (*Un candidat majoritaire*)
        if count > (n+1)/2 then
          true, x1
        else
          false, 0
      | (false, true) -> let count = occurences x2 arr i j in
        if count > (n+1)/2 then
          true, x2
        else
          false, 0
      | (true, true) when x1 = x2 -> true, x1  (*x est bien majoritaire*)
      | (true, true) -> let count1 = occurences x1 arr i j in
        if count1 > (n+1)/2 then
          true, x1
        else
          let count2 = occurences x2 arr i j in
            if count2 > (n+1)/2 then
              true, x2
            else
              false, 0
    in majo 0 (Array.length arr - 1)
;;

(*e*)

let postulant arr =
  let rec aux i j = match j - i with
    | 0 -> true, arr.(i), 1
    | n -> let b1, x1, cx1 = aux i (i + (n-1)/2) and b2, x2, cx2 = aux (i + (n+1)/2) j in
      match (b1, b2) with
      | (false, false) -> false, 0, 0
      | (true, false) -> true, x1, cx1 + (n+1)/4
      | (false, true) -> true, x2, cx2 + (n+1)/4
      | (true, true) when x1 = x2 -> true, x1, cx1 + cx2
      | (true, true) when cx1 > cx2 -> true, x1, (n+1)/2 + cx1 - cx2
      | (true, true) when cx1 < cx2 -> true, x2, (n+1)/2 + cx2 - cx1
      | (true, true) -> false, 0, 0 (*cx1 = cx2*)
  in aux
;; (*Optimisation de la complexité spatiale via la fonction auxilliaire*)

(*g*)

let majoritaire3 arr =
  let x = postulant arr 0 (Array.length arr - 1) in
    if occurences x arr 0 (Array.length arr - 1) > (Array.length arr) / 2 then
      (*Vérification*)
      true, x
    else
      false, 0
;;

(*Problème 3*)

let base = 1000 ;;

type nat = int list ;;

(*Question 1*)

let cons_nat c (n : nat) : nat = c::n ;;

(*Question 2*)

let add_nat (n1 : nat) (n2 : nat) : nat =
  let rec aux = function
    | (0, [], []) -> []
    | (r, [], []) -> [r]
    | (r, h::t, []) when h + r >= base -> cons_nat (h + r - base) (aux (1, t, []))
    | (r, h::t, []) -> cons_nat (h+r) t
    | (r, [], h::t) when h + r >= base -> cons_nat (h + r - base) (aux (1, t, []))
    | (r, [], h::t) -> cons_nat (h+r) t
    | (r, h::t, h'::t') when h + h' + r >= base -> cons_nat (h + h' + r - base) (aux (1, t, t'))
    | (r, h::t, h'::t') -> cons_nat (h + h' + r) (aux (0, t, t'))
  in aux (0, n1, n2)
;;

(*Question 3*)

let cmp_nat (n1: nat) (n2: nat) =
  let rec aux = function
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | h::t, h'::t' when h > h' -> 1
    | h::t, h'::t' when h < h' -> -1
    | h::t, h'::t' -> aux (t, t')
  in aux (n1, n2)
;;

(*Question 4*)

let sous_nat (n1: nat) (n2: nat) : nat =
  let rec aux = function
    | (0, [], []) -> []
    | (r, [], l) -> failwith "n1 < n2"
    | (r, h::t, []) when r > h -> (base + h - r)::(aux (1, t, []))
    | (r, h::t, []) -> (h - r)::t
    | (r, h::t, h'::t') when r + h' > h -> (base + h - h' - r)::(aux (1, t, t'))
    | (r, h::t, h'::t') -> (h - h' - r)::(aux (0, t, t'))
  in aux (0, n1, n2)
;;

(*Question 5*)

let rec div2_nat = function
  | [] -> [], 0
  | h::t -> let q, r = div2_nat t in
    cons_nat (h/2 + r * (base/2)) q, h mod 2
;;

type z = {signe: int ; nat: nat} ;;

(*Question 6*)

let neg_z z = {signe = - z.signe ; nat = z.nat} ;;

(*Question 7*)

let add_z z1 z2 = match (z1.signe, s2.signe) with
  | (1, 1) -> {signe = 1 ; nat = add_nat z1.nat z2.nat}
  | (-1, -1) -> {signe = 1} ; nat = add_nat z1.nat z2.nat}
  | (_, _) when cmp_nat z1.nat z2.nat = 0 -> {signe = 1 ; nat = []}
  | (1, -1) when cmp_nat z1.nat z2.nat > 0 -> {signe = 1; nat = sous_nat z1.nat z2.nat}
  | (1, -1) -> {signe = -1; nat = sous_nat z2.nat z1.nat}
  | (-1, 1) when cmp_nat z1.nat z2.nat > 0 -> {signe = -1; nat = sous_nat z1.nat z2.nat}
  | _ -> {signe = 1; nat = sous_nat z2.nat z1.nat}
;;

(*Question 8*)

let rec mul_puiss2_z p z = match p with
  | 0 -> z
  | _ -> let y = mult_puiss_2 (p-1) z in
    add_z y y
;;

(*Question 9*)

let rec decomp_puiss_2 z = match List.hd z.nat mod 2 with
  | 1 -> z, 0
  | let u, _ = div2_nat z.nat in (*On sait déjà que le reste vaut 0*)
    let v, k = decomp_puiss_2 {signe = z.signe ; nat = u} in
      v, k+1
;;
