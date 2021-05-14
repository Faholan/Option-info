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

(*Exercices sur les files*)

(*Exercice 1*)

let last_elem queue =
  let rec aux x =
    if Queue.is_empty queue then
      x
    else
      let y = Queue.take queue in
        aux y
  in let y = Queue.take queue in
    aux y
;;

(*Exercice 2*)

let queue_splitter queue =
  let queue1 = Queue.create () and queue2 = Queue.create () in
    let rec aux boolean =
      if Queue.is_empty queue then
        queue1, queue2
      else (
        if boolean then
          Queue.add (Queue.take queue) queue1
        else
          Queue.add (Queue.take queue) queue2
        ;
        aux (not boolean)
      )
    in aux true
;;

(*Exercice 3*)

let queue_fun queue f =
  let final = Queue.create () in
    let rec aux _ =
      if Queue.is_empty queue then
        final
      else (
        Queue.add (f (Queue.take queue)) final ;
        aux ()
      )
    in aux ()
;;

(*Exercice 4*)

let list_to_queue l =
  let queue = Queue.create () in
    let rec aux = function
      | [] -> queue
      | h::t -> (
        Queue.add h queue ;
        aux t
      )
    in aux l
;;

(*Exercice 5*)

let copy_queue_to_list queue =
  let l = ref [] in (
    Queue.iter (fun x -> l := x::!l) queue ;
    List.rev !l
  )
;;

(*
Exercice 6
  Problème de Josephus
*)

let josephus n k =
  let family = Queue.create () in (
    for i = 1 to n do
      Queue.add i family
    done ;
    let rec aux i =
      let x = Queue.take family in
        if Queue.is_empty family then
          x
        else
          if i = k then
            aux 1
          else (
            Queue.add x family ;
            aux (i+1)
          )
    in aux 1
  )
;;

(*Permutation aléatoire d'une liste*)

(*Exercice 1*)

let rec extrait i = function
  | [] -> failwith "index out of bounds"
  | h::t when i = 0 -> h, t
  | h::t -> let x, t' = extrait (i-1) t in
    x, h::t'
;;

let rec choix source but = function
  | 0 -> but
  | n -> let i = Random.int 0 (n-1) in
    let x, source2 = extrait i in
      choix source2 (x::but) (n-1)
;;

let shuffle l = choix l [] (List.length l) ;;

(*Exercice 2*)

let permutation n =
  let rec aux = function
    | i when i = n -> []
    | i -> i::(aux (i + 1))
  in shuffle (aux 0)
;;

(*Mariages stables*)

(* Exercice 1
Pour la configuration, avec n = 2,

[0, 1]  [1, 0]
[1, 0]  [0, 1]

il existe deux solutions :

0 <-> 1, 1 <-> 0 et
0 <-> 0, 1 <-> 1

Le problème n'admet donc pas de solution unique.
*)

(*Exercice 2*)

let make_rang f =
  let n = Array.length f in
    let rang = Array.make_matrix n n 0 in (
      for j = 0 to n-1 do
        for i = 0 to n-1 do
          rang.(j).((f.(j).(i))) <- i
        done
      done ;
      rang
    )
;;

(*Exercice 3*)

let make_celib n =
  let celib = Queue.create () in (
    for i = 0 to (n-1) do
      Queue.add i celib
    done ;
    celib
  )
;;

let gale_shapley h f =
  let rang = make_rang f and n = Array.length h in
    let mari = Array.make n (-1) and suiv = Array.make n 0 and celib = make_celib n in (
      while not (Queue.is_empty celib) do
        let i = Queue.take celib in
          let j = h.(i).(suiv.(i)) in (
            suiv.(i) <- suiv.(i) + 1 ;
            if mari.(j) = -1 then
              mari.(j) <- i
            else
              let k = mari.(j) in
                if rang.(j).(k) < rang.(j).(i) then
                  Queue.add i celib
                else (
                  mari.(j) <- i ;
                  Queue.add k celib
                )
          )
      done ;
      mari
    )
;;
