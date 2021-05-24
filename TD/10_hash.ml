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

(*Tables de hachage*)

let create m = Array.make m [] ;;

exception NotFound ;;

(*Partie 1 - Chaînage*)

let search1 key h dict =
  let rec aux = function
    | [] -> raise NotFound
    | (i, value)::_ when i = key -> value
    | (_, _)::t -> aux t
  in aux dict.(h key)
;;

let insert1 key value h dict =
  let hash = h key in
    dict.(hash) <- (key, value)::dict.(hash)
;;

let delete1 key h dict =
  let hash = h key in
    let rec aux = function
      | [] -> raise NotFound
      | (i, _)::t when i = key -> t
      | (i, value)::t -> (i, value)::(aux t)
    in dict.(hash) <- aux dict.(hash)
;;

let length1 dict =
  let result = ref 0 in (
    Array.iter (fun l -> result := !result + List.length l) dict ;
    !result
  )
;;

let list1 dict =
  let result = ref [] in (
    Array.iter (fun l -> result := l @ !result) dict ;
    !result
  )
;;

(*Partie 2 - Sondage linéaire*)

type ('a, 'b) entry = NIL | AVAILABLE | E of 'a * 'b ;;

let search2 key h dict =
  let rec aux i =
    match dict.(i) with
    | NIL -> raise NotFound
    | E(j, value) when j = key -> value
    | _ -> aux ((i+1) mod (Array.length dict))
  in aux (h key)
;;

let insert2 key value h dict =
  let rec aux i =
    match dict.(i) with
    | E(_, _) -> aux ((i+1) mod (Array.length dict))
    | _ -> dict.(i) <- E(key, value)
  in aux (h key)
;;

let delete2 key h dict =
  let rec aux i =
    match dict.(i) with
    | NIL -> raise NotFound
    | E(j, _) when j = key -> dict.(i) <- AVAILABLE
    | _ -> aux ((i+1) mod (Array.length dict))
  in aux (h key)
;;

let length2 dict =
  let result = ref 0 in
    let aux = function
      | E(_, _) -> result := !result + 1
      | _ -> ()
    in (
      Array.iter aux dict ;
      !result
    )
;;

let list2 dict =
  let result = ref [] in
    let aux = function
      | E(key, value) -> result := (key, value)::!result
      | _ -> ()
    in (
      Array.iter aux dict ;
      !result
    )
;;

(*Partie 3 - Sondage quadratique*)

let gen_hs m =
  Array.init m (fun i -> (fun k -> (k + i * i) mod m))
;;

let search3 key hs dict =
  let rec aux i =
    let hash = hs.(i) key in
      match dict.(hash) with
        | NIL -> raise NotFound
        | E(j, value) when j = hash -> value
        | _ -> aux (i + 1)
  in aux 0
;;

let insert3 key value hs dict =
  let rec aux i =
    let hash = hs.(i) key in
      match dict.(hash) with
        | E(_, _) -> aux (i + 1)
        | _ -> dict.(hash) <- E(key, value)
  in aux 0
;;

let delete3 key hs dict =
  let rec aux i =
    let hash = hs.(i) key in
      match dict.(hash) with
        | NIL -> raise NotFound
        | E(j, _) when j = hash -> dict.(hash) <- AVAILABLE
        | _ -> aux (i + 1)
  in aux 0
;;

let length3 = length2 ;;

let list3 = list2 ;;

(*Partie 4 - Double hachage*)

let search4 key h d dict =
  let hh = h key and dd = d key in
    let rec aux j =
      match dict.(hh + j * dd) with
        | NIL -> raise NotFound
        | E(i, value) when i = hh + j * dd -> value
        | _ -> aux (j + 1)
    in aux 0
;;

let insert4 key value h d dict =
  let hh = h key and dd = d key in
    let rec aux j =
      match dict.(hh + j * dd) with
        | E(_, _) -> aux (j + 1)
        | _ -> dict.(hh + j * dd) <- E(key, value)
    in aux 0
;;

let delete4 key h d dict =
  let hh = h key and dd = d key in
    let rec aux j =
      match dict.(hh + j * dd) with
        | NIL -> raise NotFound
        | E(i, value) when i = hh + j * dd -> dict.(hh + j * dd) <- AVAILABLE
        | _ -> aux (j + 1)
    in aux 0
;;

let length4 = length2 ;;

let list4 = list2 ;;
