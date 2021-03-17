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
  Test de monotonie sur une liste
*)

let rec _croissante = function
  | [] -> failwith "Empty list"
  | [_] -> true
  | a::b::t -> if a < b then false else _croissante (b::t)
;;

let _decroissante = function
  | [] -> failwith "Empty list"
  | [_] -> true
  | a::b::t -> if a > b then false else _croissante (b::t)
;;

let rec monotone = function
  | [] -> failwith "Empty list"
  | [_] -> true
  | a::b::t when a < b -> _croissante (b::t)
  | a::b::t when a > b -> _decroissante (b::t)
  | _::t -> monotone t
;;

(*
Exercice 2
  Tri par dénombrement
*)

let compte m l =
  let table = Array.make m 0 in
    let rec aux = function
      | [] -> table
      | i::t -> table.(i) <- table.(i) + 1; aux t
    in aux l
;;

let rec add a k l = match k with
  | 0 -> l
  | _ -> a::(add a (k-1) l)
;;


let tri m l =
  let table = compte m l in
    let rec aux = function
      | i when i = m -> []
      | i -> (add i table.(i) (aux (i+1)))
    in aux 0
;;

(*
Exercice 3
  Système monétaire
*)

let glouton l s =
  let rec _glouton s = function
    | [] -> if s = 0 then [] else failwith "No coin to pay"
    | h::t when h <= s -> h::(_glouton (s-h) (h::t))
    | h::t -> _glouton s t
  in _glouton s (List.rev l)
;;

(*
Exercice 4
  Polynômes creux
*)

(*a*)
let valuation = function
  | [] -> -1
  | (_, n)::_ -> n
;;

(*b*)

let rec coefs = function
  | [] -> []
  | (a, k)::t when a = 0 -> coefs t
  | (a, k)::t -> (a, k)::(coefs t)
;;

let degre poly =
  let rec _degre = function
    | [] -> -1
    | (a, k)::t when a = 0 -> _degre t
    | (a, k)::t -> k
  in _degre (List.rev poly)
;;

(*c*)

let rec print_polynome = function
  | [] -> print_char '0'
  | [(a, k)] ->
    print_int a ;
    print_string "X^" ;
    print_int k
  | (a, k)::t ->
    print_polynome t ;
    print_string " + " ;
    print_int a ;
    print_string "X^" ;
    print_int k
;;

(*d*)

let rec poly_sum p1 p2 = match p1 with
  | [] -> p2
  | (a, k)::t -> match p2 with
    | [] -> p1
    | (a', k')::t' when k < k' -> (a, k)::(poly_sum t p2)
    | (a', k')::t' when k > k' -> (a', k')::(poly_sum p1 t')
    | (a', k')::t' when a = -a' -> poly_sum t t'
    | (a', k')::t' -> (a + a', k)::(poly_sum t t')
;;

let rec poly_mult p1 p2 = match p1 with
  | [] -> []
  | (a, k)::t -> let rec multiplier = function
    | [] -> []
    | (a', k')::t' when a' = 0 -> multiplier t'
    | (a', k')::t' -> (a * a', k + k')::(multiplier t')
  in poly_sum (multiplier p2) (poly_mult t p2)
;;

(*e*)

let rec _puissance x = function
  | 0 -> 1
  | n -> let y = _puissance x (n/2) and z = (
    if n mod 2 = 0 then 1 else x
  ) in z * y * y
;;

let rec eval x = function
  | [] -> 0
  | (a, k)::t -> a * (_puissance x k) + eval x t
;;

(*f*)

(*
Exercice 5
*)

(*a*)

let croissant t =
  let current, current_max, value = ref 1, ref 0, ref t.(0) in
    for i = 1 to (Array.length t - 1) do
      if t.(i) >= !value then
        current := !current + 1
      else
        begin
          value := t.(i) ;
          current_max := max !current !current_max ;
          current := 1
        end ;
      value := t.(i)
    done ;
  max !current !current_max
;;

let croissant_lst t =
  let current, current_max, values, values_max = ref 1, ref 0, ref [t.(0)], ref [] in
    for i = 1 to (Array.length t - 1) do
      if t.(i) >= List.hd !values then
        begin
          values := t.(i)::!values ;
          current := !current + 1
        end
      else
        begin
          if !current > !current_max then
            begin
              values_max := !values ;
              current_max := !current
            end ;
          current := 1 ;
          values := [t.(i)]
        end
    done ;
    if !current > !current_max then
      !values
    else
      !values_max
;;
