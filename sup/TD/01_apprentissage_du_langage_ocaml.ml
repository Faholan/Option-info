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
  Indiquer le type des expressions suivantes et le résultat du calcul
*)
11 - 4 + 25 ;; (*int - 32*)
12.0 +. 7.1 ;; (*float - 19.1*)
let a = false in a || (3>0) ;; (*boolean - true*)
let a = true and b = false in a || b, a && b ;; (*bool * bool - true, false*)
"bon"^"soir" ;; (*string - "bonsoir"*)
print_string "bonsoir" ;; (*unit - ()*)
37 mod 3 + 7/5 ;; (*int - 2*)

(*
Exercice 2
  Comparer les différents codes suivants
*)
let x = ref 1 in
  while !x > 0 do (*Attention ! Boucle infinie !*)
    x := !x + 1
  done ;
  print_int !x
;;

let x = ref 1 in
  while !x > 0 do (*Attention, encore une boucle infinie*)
    print_int !x ;
    print_newline() ;
    x := !x + 1
  done
;;
(*Ecrit les entiers de 1 à l'infini, 1 par ligne*)
let x = ref 0 in
  while !x > 0 do (*Pas exécuté*)
    x := !x + 1
  done ;
  print_int !x (*Affiche 0*)
;;
(*Enfin un script qui termine !*)

(*
Exercice 3
  Indiquer le résultat des instructions suivantes.
  Quelles sont les valeurs de a et b à la fin ?
*)
let a = 25 and b = 15 ;; (*a = 25 et b = 15*)
(*Les instructions de type let var = value in expr sont purement locales et ne modifient pas les variables globales*)
let b = a in b+6 ;; (*int - 31*)
let a = b in a+5 ;; (*int - 20*)
let b = a+1 ;; (*int - 26*)
(*a = 25 et b = 26*)

(*
Exercice 4
*)

(*1 - Ecrire une fonction permutant circulairement un triplet*)
let permut (a, b, c) = b, c, a ;;

(*2 - Ecrire une fonction, à l'aide du module random, permutant aléatoirement un triplet*)
let permutrand (a, b, c) =
  match Random.int(6) with
    | 0 -> (a, b, c)
    | 1 -> (a, c, b)
    | 2 -> (b, a, c)
    | 3 -> (b, c, a)
    | 4 -> (c, a, b)
    | _ -> (c, b, a)
;;

(*
Exercice 5
*)

(*1 - Ecrire une fonction prenant trois entiers en argument et renvoyant le plus petit, puis une fonction renvoyant l'élément médian*)
let _min a b = if a <= b then a else b ;;
let minimal a b c =
  let d = _min a b in
    _min d c
;;

(*2 - Ecrire une fonction renvoyant l'élément médian*)
let median a b c =
  let d = minimal a b c in (
    if d = a then
      _min b c
    else (
      if d = b then
        _min a c
      else
        _min a b
      )
    ;
  )
;;

(*3 - En déduire une fonction triant trois entiers dans l'ordre croissant*)
let _max a b = if a >= b then a else b;;
let maximal a b c =
  let d = _max a b in
    _max d c
;;

let tri a b c = (minimal a b c, median a b c, maximal a b c) ;;

(*
Exercice 6
  Déterminer ce que fait cette séquance d'instruction et en donner une version plus courte
*)
let x = ref 0 ;;
while !x + 6 < 100 do
  x := !x + 6;
  done;
print_int(!x) ;;
(*Ce code écrit le plus grand multiple de 6 inférieur strictement à 100*)
(*Version courte :*)
print_int(96) ;;

(*
Exercice 7
  Indiquer le type des fonctions suivantes
*)
let f x y z = x = y + z ;; (*int -> int -> int -> bool*)
let f = fun x y -> x mod y > 2 * y + 5 ;; (*int -> int -> bool*)
let f g h x y z = (g(x,y)) *. (h y z) ;; (*('a * 'b -> float) -> ('b -> 'c -> float) -> 'a -> 'b -> 'c -> float*)

(*
Exercice 8
  Ecrire une fonction calculant la somme des chiffres d'un entier en base 10
*)

let rec sommechiffres n = if n < 10 then n else n mod 10 + (sommechiffres (n/10)) ;;

(*
Exercice 9
  Ecrire une fonction calculant le produit des nombres ompairs de 1 à n
*)
let rec produit n =
  match n mod 2 with
    | 1 -> if n > 1 then n * produit (n - 2) else 1
    | _ -> if n > 2 then produit (n - 1) else 1
;;

(*
Exercice 10
  Ecrire une fonction calculant le multiple de 3 le plus proche
*)
let multiple3 integer =
  match integer mod 3 with
    | 1 -> integer - 1
    | 2 -> integer + 1
    | _ -> integer
;;

(*
Exercice 11
  Définir un type réel réunissant entier et flottants, avec les opérations + et *, et <=
*)
type reel = N of int | F of float ;;
let _to_float = function
  | N n -> float_of_int n
  | F x -> x
;;
let _to_real x = if floor x = ceil x then N(int_of_float x) else F(x) ;;
let real_add x y = _to_real ((_to_float x) +. (_to_float y)) ;;
let real_times x y = _to_real ((_to_float x) *. (_to_float y)) ;;
let real_leq x y = (_to_float x) <= (_to_float y) ;;

(*
Exerice 12
  Type complexe
*)
type complexe = {re: float ; im : float} ;;
let conj z = {re = z.re : im = - z.im} ;;
let module z = sqrt (z.re ** 2. +. z.im ** 2.) ;;
let mult z1 z2 = {re = z1.re *. z2.re -. z1.im * z2.im ; im = z1.re *. z2.im +. z1.im * z2.re} ;;
let rec puissance z n = match n with
  | 0 -> {re = 1. : im = 0.}
  | 1 -> z
  | _ -> mult z (puissance z (n-1))
;;

(*
Exercice 13
  Somme des carrés de 1 à n
*)
let sqr_for n =
  let total = ref 0 in
    for i = 1 to n do
      total := !total + i * i
    done;
    !total
;;

let req sqr_req n = match n with
  | 0 -> 0
  | _ -> n * n + sqr_req (n - 1)
;;

let sqr_final n = n * (n+1) * (2 * n + 1) / 6 ;;
