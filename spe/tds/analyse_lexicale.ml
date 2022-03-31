(*TD Analyse lexicale

Copyright (C) 2022  Faholan <https://github.com/Faholan>
*)

type lexem = INT | FLOAT | VAR | OP | NOP | UNKNOWN ;;

type noeud = {
  etat: lexem ;
  fils: ((char -> bool) * int) list ;
} ;;

exception Blocage ;;

(*Typage*)
let est_chiffre x = String.contains "1234567890" x ;;
let est_lettre x = String.contains "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" x ;;

let abr = [|
  { (*0*)
    etat = UNKNOWN ;
    fils = [
      (est_chiffre, 1) ;
      ((fun x -> x = 'n'), 4) ;
      (est_lettre, 7) ;
      ((fun x -> x = '<'), 8) ;
      ((fun x -> x = '+' || x = '*'), 9) ;
    ] ;
  } ;
  { (*1*)
    etat = INT ;
    fils = [
      est_chiffre, 1 ;
      (fun x -> x = ','), 2 ;
    ] ;
  } ;
  { (*2*)
    etat = UNKNOWN ;
    fils = [est_chiffre, 3] ;
  } ;
  { (*3*)
    etat = FLOAT ;
    fils = [est_chiffre, 3]
  } ;
  { (*4*)
    etat = VAR ;
    fils = [
      (fun x -> x = 'o'), 5 ;
      est_lettre, 7 ;
      est_chiffre, 7 ;
      (fun x -> x = '_'), 7 ;
    ]
  } ;
  { (*5*)
    etat = VAR ;
    fils = [
      (fun x -> x = 'p'), 6 ;
      est_lettre, 7 ;
      est_chiffre, 7 ;
      (fun x -> x = '_'), 7 ;
    ]
  } ;
  { (*6*)
    etat = NOP ;
    fils = [
      est_lettre, 7 ;
      est_chiffre, 7 ;
      (fun x -> x = '_'), 7 ;
    ]
  } ;
  { (*7*)
    etat = VAR ;
    fils = [
      est_lettre, 7 ;
      est_chiffre, 7 ;
      (fun x -> x = '_'), 7 ;
    ]
  } ;
  { (*8*)
    etat = UNKNOWN ;
    fils = [(fun x -> x = '-'), 9]
  } ;
  { (*9*)
    etat = OP ;
    fils = []
  } ;
|]
;;


let delta abr state c =
  let rec evaluator = function
    | [] -> raise Blocage
    | (pred, s)::t -> if pred c then s else evaluator t
  in
    evaluator abr.(state).fils
;;


let simule abr i mot =
  let j = ref 0 and etat = ref 0 and dernier_final = ref UNKNOWN and dernier_j = ref 0 in
    let rec avance () =
      if i + !j < String.length mot then
        try
          (
            etat := delta abr !etat mot.[i + !j] ;
            if abr.(!etat).etat <> UNKNOWN then (
              dernier_final := abr.(!etat).etat ;
              dernier_j := !j
            ) ;
            incr j ;
            avance ()
          )
        with
          Blocage -> !dernier_final, 1 + !dernier_j
      else
        !dernier_final, 1 + !dernier_j
    in
      avance ()
;;

let analyse_lex abr mot =
  let i = ref 0 and res = ref [] and n = String.length mot in
    while !i < n do
      let etat, j = simule abr !i mot in
        if etat <> UNKNOWN then
          res := (String.sub mot !i j, etat)::!res ;
        i := !i + j
    done ;
    List.rev !res
;;


analyse_lex abr "123abc def 3,14 abc123 abc_123" ;;

analyse_lex abr "var2<-12,3+var1;nop;var1<-var2+42" ;;

(*
Références :

(Introduction à la ??) calculabilité - Olivier Carton (+ théorique)
Introduction à la calculabilité - Pierre Wolper (- théorique, mais avec qqs fautes)
  -> Fonctions primitives et primitives récursives
Compilateurs - Aho Sethi Ulimal Liam
  -> Analyse LL(1) et SLR
Introduction à l'algorithmique - Cormen
  -> Graphes
  -> Knuth Morris Pratt
*)
