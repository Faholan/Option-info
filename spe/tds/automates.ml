(*
TD sur les automates

Copyright (C) 2022  Faholan <https://github.com/Faholan>
*)

type af = {
  etats: int list ; (* ensemble des états *)
  q0: int;          (* état initial *)
  finaux: int list; (* liste des états finaux *)
  (* la fonction de transition est représentée par la liste des
     triplets (q, a, q’) tels que delta(q, a) = q’ : *)
  transitions: (int * char * int) list;
};;

(*Question 1*)

let a_n = {
  etats = [0];
  q0 = 0;
  finaux = [0];
  transitions = [(0, 'a', 0)];
} ;;

(*Question 2*)
let mot_vide = {
  etats = [0];
  q0 = 0;
  finaux = [0];
  transitions = [] ;
} ;;

(*Question 3*)
let langage_vide = {
  etats = [0; 1];
  q0 = 0;
  finaux = [1];
  transitions= [];
} ;;

(*Question 4*)
exception Blocage ;;

let delta automate etat chr =
  let rec matcher = function
    | (q, a, q')::_ when q = etat && a = chr -> q'
    | _::t -> matcher t
    | [] -> raise Blocage
  in matcher automate.transitions
;;

let execute automate chrs =
  let rec executor etat = function
    | c::t -> executor (delta automate etat c) t
    | [] -> etat
  in executor automate.q0 chrs
;;

(*Question 5*)
let reconnait automate chrs =
  try
    List.mem (execute automate chrs) automate.finaux
  with
    | Blocage -> false
;;

reconnait a_n ['a'; 'a'; 'a'; 'a'] ;;
not (reconnait a_n ['b'; 'a'; 'r']) ;;
reconnait mot_vide [] ;;
not (reconnait mot_vide ['f']) ;;
not (reconnait langage_vide ['f'; 'o'; 'o']) ;;

(*Question 6*)
let est_complet automate alphabet =
  let rec test_alpha = function
    | a::t -> test_states a automate.etats; test_alpha t
    | [] -> ()
  and test_states a = function
    | n::t -> ignore (delta automate n a); test_states a t
    | [] -> ()
  in
    try
      test_alpha alphabet ;
      true
    with
      | Blocage -> false
;;

(*
Question 7

Tout automate fini déterministe est équivalent à l'automate auquel on a ajouté un état poubelle,
vers lequel toutes les transitions inexistantes pointent, y compris celles partant de lui-même
*)

(*Question 8*)

let rec transitions_poubelle poubelle current = function
  | h::t -> transitions_poubelle poubelle ((poubelle, h, poubelle)::current) t
  | [] -> current
;;

let complete alphabet automate =
  let poubelle = List.length automate.etats and transitions = ref automate.transitions in
    let rec process_alpha = function
      | a::t -> process_states a automate.etats; process_alpha t
      | [] -> ()
    and process_states a = function
      | n::t -> (
        (
          try
            ignore (delta automate n a)
          with
            | Blocage -> transitions := (n, a, poubelle)::!transitions
        ) ;
        process_states a t
      )
      | [] -> ()
    in
      process_alpha alphabet ;
      {
        etats = poubelle::automate.etats ;
        q0 = automate.q0 ;
        finaux = automate.finaux ;
        transitions = transitions_poubelle poubelle !transitions alphabet
      }
;;

(*Question 9

Si un mot est reconnu par A1 x A2, cela signifie qu'il est reconnu par A1 et par A2
car on doit avoir δ*(q1, mot) final pour A1 et δ*(q2, mot) final pour A2.

Réciproquement, un mot reconnu par A1 et A2 est reconnu par A1xA2
*)

(* Question 10
L'application i, j -> i * q + j est une bijection.
*)

let code q (i, j) = i * q + j ;;

let decode q n = (n / q, n mod q) ;;


(*Question 11*)
let get_etats q etats1 etats2 =
  let rec builder current el1 state1 = function
    | h::t -> builder ((code q (el1, h))::current) el1 state1 t
    | [] -> (match state1 with
      | h::t -> builder current h t etats2
      | [] -> current
    )
  in builder [] (List.hd etats1) (List.tl etats1) etats2
;;

let get_transitions q transitions1 transitions2 =
  let rec first_filler current = function
    | (p, a, p')::t -> first_filler (second_filler current p a p' transitions2) t
    | [] -> current
  and second_filler current p a p' = function
    | (p2, a2, p2')::t when a = a2 -> second_filler ((code q (p, p2), a, code q (p', p2'))::current) p a p' t
    | _::t -> second_filler current p a p' t
    | [] -> current
  in first_filler [] transitions1
;;

let produit automate1 automate2 =
  let q = List.length automate2.etats in
    {
      etats = get_etats q automate1.etats automate2.etats ;
      q0 = code q (automate1.q0, automate2.q0) ;
      finaux = List.fold_left2 (fun cur q1 q2 -> (code q (q1, q2))::cur) [] automate1.finaux automate2.finaux ;
      transitions = get_transitions q automate1.transitions automate2.transitions;
    }
;;

(*Question 12*)
let accessibles automate =
  let accedes = Array.make (List.length automate.etats) false in
    let rec access sommet =
      if not accedes.(sommet) then (
        accedes.(sommet) <- true ;
        List.iter (
          fun (q, _, q') -> (
            if q = sommet then
              access q'
          )
        ) automate.transitions
      )
    in
      access automate.q0 ;
      List.fold_left (
        fun current q -> if accedes.(q) then
          q::current
        else
          current
      ) [] automate.etats
;;

(*Question 13*)
let coaccessibles automate =
  let accedes = Array.make (List.length automate.etats) false in
    let rec access sommet =
      if not accedes.(sommet) then (
        accedes.(sommet) <- true ;
        List.iter (
          fun (q, _, q') -> (
            if q' = sommet then
              access q
          )
        ) automate.transitions
      )
    in
      List.iter access automate.finaux ;
      List.fold_left (
        fun current q -> if accedes.(q) then
          q::current
        else
          current
      ) [] automate.etats
;;

(*Question 14

L'automate émondé obtenu à partir de A est l'automate comportant uniquement les sommets
accessibles et coaccessibles de A, ainsi que les transitions conçernant ces sommets
*)

(*Question 15*)

let emonde_sommets acc coacc q0 =
  let n = List.length acc and newq0 = ref 0 in
    let newtags = Array.make n 0 and conserves = Array.make n false and tag = ref 0 in
      for i = 0 to n - 1 do
        if i = q0 then (
          newq0 := !tag ;
        ) ;
        if i = q0 || (List.mem i acc && List.mem i coacc) then
          newtags.(i) <- !tag ;
          conserves.(i) <- true ;
          tag := !tag + 1
      done ;
      newtags, conserves, !newq0, !tag
;;

let emonde automate =
  let acc = accessibles automate and coacc = coaccessibles automate in
    let newtags, conserves, newq0, newlen = emonde_sommets acc coacc automate.q0 in
    {
      etats = List.init newlen (fun x -> x) ;
      q0 = newq0 ;
      finaux = List.fold_left (
        fun current q -> if conserves.(q) then
          newtags.(q)::current
        else
          current
      ) [] automate.finaux ;
      transitions = List.fold_left (
        fun current (q, a, q') -> if conserves.(q) && conserves.(q') then
          (newtags.(q), a, newtags.(q'))::current
        else
          current
      ) [] automate.transitions
    }
;;

(*Question 16*)
let get_alphabet automate =
  let rec fetcher current = function
    | (q, a, q')::t when List.mem a current -> fetcher current t
    | (q, a, q')::t -> fetcher (a::current) t
    | [] -> current
  in fetcher [] automate.transitions
;;

let rec next_state automate chr = function
  | [] -> []
  | h::t -> try
    (delta automate h chr)::(next_state automate chr t)
  with
    | Blocage -> next_state automate chr t
;;

let index elem =
  let rec _index cur = function
    | h::t when h = elem -> cur
    | h::t -> _index (cur + 1) t
    | [] -> failwith "Element not in list"
  in _index 0
;;

let rec in_common l1 = function
  | h::t when List.mem h l1 -> true
  | h::t -> in_common l1 t
  | [] -> false
;;

let rec determine automate =
  let alphabet = get_alphabet automate and sommets = ref [[automate.q0]] and aretes = ref [] and todo = Queue.create () in
    Queue.push [automate.q0] todo ;
    let rec treat () =
      if Queue.is_empty todo then
        ()
      else (
        let state = Queue.pop todo in
          let rec treat_chr = function
            | [] -> ()
            | h::t -> (
              let newstate = next_state automate h state in
                if not (List.mem newstate !sommets) then (
                  sommets := newstate::!sommets ;
                  Queue.push newstate todo
                ) ;
                aretes := (state, h, newstate)::!aretes
            ); treat_chr t
          in
            treat_chr alphabet ;
            treat ()
      )
    in
      treat () ;
      {
        etats = List.init (List.length !sommets) (fun x -> x) ;
        q0 = index [automate.q0] !sommets ;
        finaux = List.fold_left (
          fun current candidate -> if in_common candidate automate.finaux then
            (index candidate !sommets)::current
          else
            current
        ) [] !sommets ;
        transitions = List.fold_left (
          fun current (q1, a, q2) -> ((index q1 !sommets), a, (index q2 !sommets))::current
        ) [] !aretes
      }
;;

(*Question 17*)
let minimise auto =
  let automate = emonde auto in
    let groupes = Array.make (List.length automate.etats) 0 in
      for i = 0 to List.length automate.etats - 1 do
        if List.mem i automate.finaux then
          groupes.(i) <- 1 (*Séparation initiale*)
      done ;
;;

(*Question 18*)
let afd_local prefixes suffixes facteurs =
  let counter = ref 2 in
    {
      etats = List.init (3 + List.length facteurs) (fun x -> x) ;
      q0 = 0;
      finaux = [2] ;
      transitions = List.fold_left (
        fun current chr -> (0, chr, 1)::current
      ) (
        List.fold_left (
          fun current chr -> (1, chr, 2)::current
        ) (
          List.fold_left (
            fun current (chr1, chr2) -> (
              counter := !counter + 1 ;
              (1, chr1, !counter)::(!counter, chr2, 1)::current
            )
          ) [] facteurs
        ) suffixes
      ) prefixes
    }
;;
