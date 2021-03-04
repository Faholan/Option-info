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

let rec redondances = function
  [] -> []
  | h::t -> h::(redondances (remove h t))
;;

let rec aplati = function
  [] -> []
  | h::t -> h @ (aplati t)
;;

let rec rev = function
  [] -> []
  | h::t -> rev t @ [h]
;;

let rec iter f = function
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

let rec find p = function
  [] -> failwith "not found"
  | h::t when p h -> h
  | h::t -> find p t
;;

let rec filter p = function
  [] -> []
  | h::t when p h -> h::(filter p t)
  | h::t -> filter p t
;;

let rec assoc a = function
  [] -> failwith "not found"
  | (b, c)::t when b = a -> c
  | _::t -> assoc a t
;;

let rec split = function
  [] -> [], []
  | (a, b)::t -> let c, d = split t in
    a::c, b::d
;;

let rec merge ord l = function
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

let decomp = function
  | n when n < 2 -> []
  | n -> _decomp n 2
;;

(*b - fonction isprime*)

let isprime n = List.length (decomp n) = 1 ;;

(*c - nombres premiers inférieurs à 1000*)
let rec intervalle a b =
  if a > b then
    []
  else
    a::(intervalle (a+1) b)
;;

(*Méthode 1 peu efficace*)
let nb_premiers n = List.filter isprime (intervalle 2 n) ;;

(*Méthode 2, crible d'Eraosthème*)
let rec _eratostheme = function
  [] -> []
  | h::t -> h::(_eratostheme (List.filter (fun x -> x mod h <> 0) t))
;;

let eratostheme n = _eratostheme (intervalle 2 n) ;;

(*
[
  2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71;
  73; 79; 83; 89; 97; 101; 103; 107; 109; 113; 127; 131; 137; 139; 149; 151;
  157; 163; 167; 173; 179; 181; 191; 193; 197; 199; 211; 223; 227; 229; 233;
  239; 241; 251; 257; 263; 269; 271; 277; 281; 283; 293; 307; 311; 313; 317;
  331; 337; 347; 349; 353; 359; 367; 373; 379; 383; 389; 397; 401; 409; 419;
  421; 431; 433; 439; 443; 449; 457; 461; 463; 467; 479; 487; 491; 499; 503;
  509; 521; 523; 541; 547; 557; 563; 569; 571; 577; 587; 593; 599; 601; 607;
  613; 617; 619; 631; 641; 643; 647; 653; 659; 661; 673; 677; 683; 691; 701;
  709; 719; 727; 733; 739; 743; 751; 757; 761; 769; 773; 787; 797; 809; 811;
  821; 823; 827; 829; 839; 853; 857; 859; 863; 877; 881; 883; 887; 907; 911;
  919; 929; 937; 941; 947; 953; 967; 971; 977; 983; 991; 997
]
*)

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
