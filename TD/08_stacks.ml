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

(*Exercice 1*)

let echange t1 t2 =
  let e1 = Stack.pop t1 and e2 = Stack.pop t2 in
    begin
      Stack.push e1 t2 ;
      Stack.push e2 t1
    end
;;

(*Exercice 2*)

(*a*)

let list_to_stack l =
  let st = Stack.create () in
    let rec aux = function
      | [] -> st
      | h::t -> (
        Stack.push h st ;
        aux t
      )
    in aux (List.rev l)
;;

(*b*)

let stack_to_list st =
  let rec aux = function
    () -> if Stack.is_empty st then
      []
    else
      let e = Stack.pop st in
        e::(aux ())
  in aux ()
;;

(*c*)

let copy_stack_to_list st =
  let st' = Stack.create () in
    begin
      Stack.iter (fun x -> Stack.push x st') st ;
      List.rev (stack_to_list st') (*Sinon les résultats sont inversés*)
    end
;;

(*Exercice 3*)

let max_stack st =
  let rec aux _ = match Stack.length st with
    | 0 -> failwith "Empty stack"
    | 1 -> Stack.pop st
    | _ -> let e = Stack.pop st in
      max e (aux ())
  in aux ()
;;

let max_min_naif st =
  let cur_max = ref (Stack.pop st) in
    let cur_min = ref !cur_max in
      while not (Stack.is_empty st) do
        let e = Stack.pop st in
          if e > !cur_max then
            cur_max := e
          else
            if e < !cur_min then
              cur_min := e
      done ;
      !cur_max, !cur_min
;;

(*On suppose que la longueur du stack est une puissance de 2*)
let rec max_min st =
  let aux sta =
    let sta_min = Stack.create () and sta_max = Stack.create () in
      let rec auxaux _ =
        if Stack.is_empty sta then
          sta_max, sta_min
        else
          let e1 = Stack.pop sta in
            let e2 = Stack.pop sta in
              begin
                if e1 > e2 then
                  begin
                    Stack.push e1 sta_max ;
                    Stack.push e2 sta_max
                  end
                else
                  begin
                    Stack.push e1 sta_min ;
                    Stack.push e2 sta_max
                  end
                ;
                auxaux ()
              end
      in auxaux ()
  in
    match Stack.length st with
    | 0 -> failwith "Empty stack"
    | 1 -> let e = Stack.pop st in
      e, e
    | _ -> let sta_max, sta_min = aux st in
      let e_max, _ = max_min sta_max in
        let _, e_min = max_min sta_min in
          e_max, e_min
;;

(*
Avec le premier algorithme, dans le pire des cas on effectue, pour chaque élément,
2 comparaisons. La complexité est donc exponentielle, en O(2^n)

Avec le deuxième algorithme, on effectue n/2 comparaisons, et 2 appels récursifs
sur une donnée de taille n/2

L'équation de complexité est donc de la forme :

T(n) = 2T(n/2) + 0(n/2)
La complexité est donc en O(n*log(n))
La version récursive a une complexité temporelle bien supérieure à la méthode naïve.
*)

(*Exercice 4*)

let premiere_occurrence x st =
  let aux pos = function
    | [] -> 0
    | h::t when h = x -> pos
    | h::t -> aux (pos + 1) t
  in aux 1 (copy_stack_to_list st)
;;

(*Exercice 5*)

let first_to_last st =
  let x = Stack.pop st in
    let rec aux _ = if Stack.is_empty st then
      Stack.push x st
    else
      let y = Stack.pop st in (
        aux () ;
        Stack.push y st
      )
    in aux ()
;;

(*Exercice 6*)

(*a*)

let destroy_stack_to_last st =
  let rec aux _ = let e = Stack.pop st in
    if Stack.is_empty st then
      e
    else
      aux ()
  in aux ()
;;

(*b*)

let stack_to_last st =
  let st' = Stack.create () in
    begin
      Stack.iter (fun x -> Stack.push x st') st ;
      Stack.pop st'
    end
;;

(*Exercice 7*)
let split_stack st =
  let st1 = Stack.create () and st2 = Stack.create () in
    let rec aux val =
      if Stack.is_empty st then
        st1, st2
      else (
        if val then
          Stack.push (Stack.pop st) st1
        else
          Stack.push (Stack.pop st) st2
        ;
        aux (not val)
      )
    in aux true
;;

(*Exercice 8*)

let shuffle st1 st2 =
  let st = Stack.create () in
    let rec aux _ = match Stack.is_empty st1, Stack.is_empty st2 with
      | (true, true) -> st
      | (true, false) -> (
          Stack.push (Stack.pop st2) st ;
          aux ()
        )
      | (false, true) -> (
          Stack.push (Stack.pop st1) st ;
          aux ()
        )
      | (false, false) -> (
          if Random.bool () then
            Stack.push (Stack.pop st1) st
          else
            Stack.push (Stack.pop st2) st
          ;
          aux ()
        )
    in aux ()
;;

(*Exercice 9*)

let double_parenthesage str =
  let parenthesis = Stack.create () and bracket = Stack.create () in
    let rec aux i =
      if i = String.length str then
        if not (Stack.is_empty parenthesis) then
          failwith "Unmatched opening parenthesis"
        else
          if not (Stack.is_empty parenthesis) then
            failwith "Unmatched opening bracket"
          else
            [], []
      else
        match str.[i] with
          | '(' -> (
              Stack.push i parenthesis ;
              let l, l' = aux (i+1) in
                l, l'
            )
          | '[' -> (
              Stack.push i bracket ;
              let l, l' = aux (i+1) in
                l, l'
            )
          | ')' -> if Stack.is_empty parenthesis then
            failwith "Unmatched closing parenthesis"
          else (
            let j = Stack.pop parenthesis in
              let l, l' = aux (i+1) in
                (j, i)::l, l'
          )
          | ']' -> if Stack.is_empty bracket then
            failwith "Unmatched closing bracket"
          else (
            let j = Stack.pop bracket in
              let l, l' = aux (i+1) in
                l, (j, i)::l'
          )
          | _ -> failwith "Unrecognized character"
    in let l, l' = aux 0 in
      (List.rev l), (List.rev l')
;;
