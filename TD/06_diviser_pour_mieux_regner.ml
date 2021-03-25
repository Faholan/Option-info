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

(*
Exercice 1
  Une fonction calculant l'indice du deuxième plus grand élément d'un tableau
*)

let deuxieme t =
  let i1 = ref 0 and i2 = ref 0 in
    for i = 1 to Array.length t do
      if t.(i) > t.(!i1) then (
        i1 := i ;
        i2 := !i1
      ) else
        if t.(i) > t.(!i2) then
          i2 := i
    done ;
    !i2
;;
(*2n comparaisons au maximum.*)

(*
Exercice 2
  Tri par retournement de préfixe
*)

(*a*)
let retourne t i =
  for j = 0 to i/2 do
    let aux = t.(j) in
      t.(j) <- t.(i - j) ;
      t.(i - j) <- aux
  done
;;
(*Complexité : O(i)*)

(*b*)
(*Il faut retoruner la liste jusqu'à l'indice du plus grand élément, puis toute la liste*)

(*c*)
let tri_retourne t =
  for n = (Array.length t - 1) downto 1 do
    let i = ref 0 in
      for j = 1 to n do
        if t.(j) > t.(!i) then
          i := j
      done ;
      retourne t !i ;
      retourne t n
  done ;
  t
;;

(*
Exercice 3
  Couleurs
*)

type couleur = Rouge | Blanc | Bleu ;;

let color_to_int = function
  | Rouge -> 1
  | Blanc -> 2
  | Bleu -> 3
;;

(*a*)
let rec modification = function
  | [] -> [], false
  | [a] -> [a], false
  | a::b::t when (color_to_int a) > (color_to_int b) ->
    let l, _ = modification (a::t) in
      b::l, true
  | a::b::t -> let l, val = modification (b::t) in
    b::l, val
;;

(*b*)
let rec tri_drapeau l =
  let processed, test = modification l in
    if test then
      tri_drapeau processed
    else
      processed
;;

(*
Exercice 4
  Médiane
*)

(*a*)
(*
si m1 et m2 étaient deux médianes d'un ensemble, avec m1 < m2, et
F1 = {x ∈ E | x < m1}, F2 = {x ∈ E | x < m2} sont par définition tels que card F1 = card F2 = n/2
Mais, comme m1 < m2, on a :
F1 ⊂ F2, m1 ∈ F2, m1 ∉ F1. Comme F1 et F2 sont de cardinal fini, on a donc :
card F2 ≥ card F1 + 1
d'où n/2 ≥ n/2 + 1, ce qui est absurde.

La médiane d'un ensemble est donc unique.
*)


(*b*)

(*quicksort*)
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

let mediane t =
  tri_rapide t ;
  t.((Array.length t) / 2)
;;

(*c*)
let mediane_2 t1 t2 =
  let i1 = ref 0 and i2 = ref 0 in
    while !i1 + !i2 < (Array.length t1) - 1 do
      if t1.(!i1) < t2.(!i2) then
        incr i1
      else
        incr i2
    done ;
    if t1.(!i1) < t2.(!i2) then
      t2.(!i2)
    else
      t1.(!i1)
;;

(*
Exercice 5
  Le problème de la sélection
*)

(*a*)

let selection_1 t k =
  tri_rapide t ;
  t.(k)
;;

(*b*)

let selection_2 t k =
  let last_max = ref t.(0) in
    for j = 0 to k do
      let current_max = ref t.(0) in
        for i = 1 to (Array.length t - 1) do
          if (j = 0 || t.(i) < !last_max) && t.(i) > !current_max then
            current_max := t.(i)
        done ;
        last_max := !current_max
    done ;
    !last_max
;;

(*c*)

let partition t i j =
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
    !g - i - 1
;;

(*d*)

let selection_3 t k =
  let rec selection_aux i j f =
    let d = partition t i j in
      if d = f then
        t.(i + d)
      else
        if d > f then
          selection_aux i (i + d - 1) f
        else
          selection_aux (i + d + 1) j (f - d - 1)
  in selection_aux 0 (Array.length t - 1) (k - 1) (*Numérotation à partir de 0 en interne*)
;;

(*
Le pire des cas correspond à k = 1 et, à chaque itération, d = 0

On a alors :

T(n) = n - 1 + T(n-1) = sum(i = 1, n, i - 1) = n(n+1)/2 - n = n(n-1)
La complexité dans le pire des cas est en O(n²)
*)
