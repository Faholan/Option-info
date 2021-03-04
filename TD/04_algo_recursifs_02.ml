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
  Fonctions sur les listes
*)


let remove elem l = List.filter (fun x -> x <> elem) l ;;

let rec redondances l = match l with
  [] -> []
  | h::t -> h::(redondances (remove h t))
;;

let rec aplati l = match l with
  [] -> []
  | h::t -> h @ (aplati t)
;;

let rec rev l = match l with
  [] -> []
  | h::t -> rev t @ [h]
;;

let rec iter f l = match l with
  [] -> ()
  | h::t -> begin
    f h ;
    iter f t
  end
;;

let rec fold_left f l a = match l with
  [] -> failwith "empty list"
  | [b] -> f a b
  | h::t -> f (fold_left f t a) h
;;

let rec find p l = match l with
  [] -> failwith "not found"
  | h::t when p h -> h
  | h::t -> find p t
;;

let rec filter p l = match l with
  [] -> []
  | h::t when p h -> h::(filter p t)
  | h::t -> filter p t
;;

let rec assoc a l = match l with
  [] -> failwith "not found"
  | (b, c)::t when b = a -> c
  | _::t -> assoc a t
;;

let rec split l = match l with
  [] -> [], []
  | (a, b)::t -> let c, d = split t in
    a::c, b::d
;;

let rec merge ord l l' = match l' with
  [] -> l
  | h'::t' -> match l with
    [] -> h'::t'
    | h::t when ord h' h <= 0 -> h'::(merge ord (h::t) t')
    | h::t -> h::(merge ord t (h'::t'))
;;

(*
Exercice 2
  Ensembles représentés par des listes
*)

let rec union a b = match a with
  [] -> b
  | h::t when List.mem h b -> union t b
  | h::t -> h::(union t b)
;;

let rec intersection a b = match a with
  [] -> []
  | h::t when List.mem h b -> h::(intersection t b)
  | _::t -> intersection t b
;;

let rec diff a b = match a with
  [] -> []
  | h::t when List.mem h b -> diff t b
  | h::t -> h::(diff t b)
;;

let rec inclus a b = match a with
  [] -> true
  | h::t when List.mem h b -> inclus t b
  | _ -> false
;;

let egal a b = inclus a b && inclus b a ;;

(*
Exercice 3
  Ecriture binaire
*)

let rec int_to_bin = function
  0 -> []
  | 1 -> [1]
  | x when x mod 2 = 0 -> int_to_bin (x/2) @ [0]
  | x -> int_to_bin (x/2) @ [1]
;;

let rec _bin_to_int = function
  [] -> failwith "empty list"
  | [0] -> 0
  | [1] -> 1
  | h::t -> h + 2 * (_bin_to_int t)
;;

let bin_to_int l = _bin_to_int (List.rev l) ;;

(*
Exercice 4
  Décomposition en facteurs premiers
*)

(*a - décomposition d'un nombre premier*)

let rec _decomp n p = match n with
  1 -> []
  | _ when n mod p = 0 -> p::(_decomp (n/p) (p+1))
  | _ -> _decomp n (p+1)
;;

let decomp n = match n with
  | _ when n < 2 -> []
  | _ -> _decomp n 2
;;

(*b - fonction isprime*)

let isprime n = List.length (decomp n) = 1 ;;

(*c - nombres premiers inférieurs à 1000 (un algorithme plus efficace est possible avec des tableaux et le crible d'Erastothème)*)
let rec intervalle a b =
  if a > b then
    []
  else
    a::(intervalle (a+1) b)
;;

let nb_premiers n = List.filter isprime (intervalle 2 n) ;;

(*
Exercice 5
  Nombres romains
*)

let _val = function
  'I' -> 1
  | 'V' -> 5
  | 'X' -> 10
  | 'L' -> 50
  | 'C' -> 100
  | 'D' -> 500
  | 'M' -> 1000
  | _ -> failwith "unkown character encountered"
;;

let rec romain str = match String.length str with
  0 -> 0
  | 1 -> _val str.[0]
  | n -> if _val str.[0] < _val str.[1] then
    romain (String.sub str 1 (String.length str - 1)) - _val str.[0]
  else
    romain (String.sub str 1 (String.length str - 1)) + _val str.[0]
;;

let rec _anagrammes beginning ending = match String.length ending with
  0 -> [beginning]
  | n -> let final = ref [] in
    for i = 0 to n - 1 do
      final := !final @ (_anagrammes (beginning ^ (String.sub ending i 0)) ((String.sub ending 0 i) ^ (String.sub ending (i + 1) (n - i - 1))))
    done ;
    !final
;;

let anagrammes str = redondances (_anagrammes str "") ;;

(*
Exercice 7
  Coefficients binomiaux
*)

(*a*)

let rec binomial p n =
  if p > n then
    0
  else
    if p = n || p = 0 then
      1
    else
        binomial p (n - 1) + binomial (p - 1) (n - 1)
;;

(*b*)

let rec next_line = function
  [] -> failwith "empty list"
  | [n] -> [n]
  | h::t -> (h + List.hd t)::(next_line t)
;;

let rec printer = function
  [] -> ()
  | h::t -> begin
    print_int h ;
    print_char ' ' ;
    printer t
  end
;;

let rec _pascal l n =
  begin
    printer l ;
    print_char '\n' ;
    if n > 0 then
      _pascal (1::(next_line l)) (n - 1)
  end
;;

let pascal n = _pascal [1] n ;;
