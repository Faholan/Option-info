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
Did you know it ?

GNU stands for "GNU is Not Unix"
brb stands for "bad recursion brb"
*)

(*
Exercice 1
  Que calcule la fonction récursive suivante ?
*)

let rec f x y = match x with
  0 -> 0
  | x when x>0 -> f (x-1) y + y
  | _ -> - f (-x) y
;;
(*Cette fonction calcule x * y*)

(*
Exercice 2
  Que calcule la fonction récursive suivante ?
*)

let rec g = function
  0 -> 0
  | n when n mod 2 = 0 -> 1 + g (n/2)
  | n -> g (n-1)
;;
(*Cette fonction calcule la plus grande puissance de 2 inférieure ou égale à n (n > 0)*)

(*
Exercice 3
  Ecrire une fonction récursive testant si un mot est un palindrome
*)

let rec palindrome str = match String.length str with
  | 0 -> true
  | 1 -> true
  | n -> str.[0] = str.[n - 1] && palindrome (String.sub str 1 (n - 2))
;;

(*
Exercice 4
  Ecrire une fonction affichant à l'écran le développement binaire d'un entier
*)

let rec dvpt_binaire integer =
  if integer <> 0 then
    begin
      dvpt_binaire (integer / 2) ;
      print_int (integer mod 2)
    end
;;

(*
Exercice 5
  Ecrire une fonction calculant le numéro correspondant à un couple d'entiers naturels
  Ecrire une fonction effectuant l'opération inverse
*)

let rec numero (a, b) = match a + b with
  | 0 -> 0
  | _ -> b + 1 + numero (0, a + b - 1)
;;

let rec _couple n current total =
  if n < total + current + 1 then
    (total + current - n, n - total)
  else
    _couple n (current + 1) (total + current + 1)
;;

let couple n = _couple n 0 0 ;;

let numero_final (p, q) = q + (p + q) * (p + q + 1) / 2 ;;

(*
Exercice 6
  Coupe minimale dans un tableau
*)

let rec _coupe_minimale start table =
  if Array.length table = 1 then
    max start (max table.(0) (start + table.(0)))
  else
    if start < 0 then
      _coupe_minimale table.(0) (Array.sub table 1 (Array.length table - 1))
    else
      _coupe_minimale (start + table.(0)) (Array.sub table 1 (Array.length table - 1))
;;

let coupe_minimale table =
  if Array.length table = 1 then
    table.(0)
  else
    _coupe_minimale table.(0) (Array.sub table 1 (Array.length table - 1))
;;

(*
Exercice 7
  Anagrames d'un mot
*)

(*a*)

let rec ana_aux beginning ending =
  if ending = "" then
    print_string (beginning ^ " ")
  else
    for i = 0 to (String.length ending - 1) do
      ana_aux (beginning ^ (String.sub ending i 1)) ((String.sub ending 0 i) ^ (String.sub ending (i + 1) (String.length ending - i - 1)))
    done ;
;;

let anagrammes str = ana_aux "" str ;;

(*b*)
(*
Si l'on note n la longueur du mot, il y en a n! (on choisit une lettre parmis les n, puis on répète cette opérations avec n - 1 lettres)
*)

(*
Exercice 8
  Programmer la suite de Thue-Morse
*)

let rec t_m = function
  0 -> 0
  | n when n mod 2 = 0 -> t_m (n/2)
  | n -> 1 - t_m (n/2)
;;

(*Cette fonction calcule la somme des chiffres de la décomposition binaire de n modulo 2*)

(*
Si n = R(k) + 2^k, avec R(k) somme de puissances de 2 supérieures à k
On note C(n) le résultat de compte
C(n) = C(R) + C(2^k)

C(2^k) = 1 si k = 0
C(2^k) = 2^(k-1) + 1 - t_m(R(k))
En effet, il y a 2^k + 1 entiers inféfieurs ou égaux à 2^k
  Parmi ceux-ci, 2^(k-1) ont 0 comme image via t_m, et 2^(k-1) + 1 ont 1 comme image
  (t_m(2^k) = 1)
  De plus t_m(R(k+1)) = 1 - t_m(R(k))
Au final, les contributions en t_m(R(k)) se compensent en t_m(n)
*)

let rec _puissance x = function
  0 -> 1
  | n -> let y = _puissance x (n/2) and z = (
    if n mod 2 = 0 then 1 else x
  ) in z * y * y
;;

let rec _compte n k =
  if n = 0 then
    0
  else
    match n mod 2 with
    | 0 -> _compte (n/2) (k + 1)
    | _ -> (_puissance 2 (k - 1)) + (_compte (n/2) (k + 1))
;;

let compte n = _compte (n/2) 1 + (n mod 2) + t_m n ;;
