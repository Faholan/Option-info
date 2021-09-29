(*
Cours sur les arbres binaires de recherche.

Copyright (C) 2021  Faholan <https://github.com/Faholan>
*)

(*
Définition :

On appelle arbre binaire de recherche tout arbre binaire dont le parcours infixe renvoie une liste d'éléments rangés par ordre croissant.
*)

(*
Conséquence :

Pour tout noeud, les étiquettes des fils gauches sont inférieures à celles du noeud et celle du fils droit lui sont supérieures.
*)

(*
Note :

En général, il convient de ne pas avoir de doublons dans l'arbre et on convient en général une définition plus stricte :

- Les étiquettes du fils gauche de tout noeud sont inférieures ou égales à l'étiquette du noeud.
- Les étiquettes du fils droit de tout noeud sont strictement supérieures à l'étiquette du noeud.
*)

type 'a abr =
  | Nil
  | Noeud of 'a * 'a abr * 'a abr
;;

let rec recherche elem = function
  | Nil -> false
  | Noeud(k, _, _) when k = elem -> true
  | Noeud(k, gauche, droit) -> recherche elem (if elem < k then gauche else droit)
;;

let rec ajout elem = function
  | Nil -> Noeud(elem, Nil, Nil)
  | Noeud(k, gauche, droit) when elem > k -> Noeud(k, gauche, ajout elem droit)
  | Noeud(k, gauche, droit) when elem < k -> Noeud(k, ajout elem gauche, droit)
  | noeud -> noeud
;;

let rec retrait_racine = function
  | Nil -> Nil
  | Noeud(_, gauche, droit) -> fusion gauche droit
and fusion fg fd = match fg, fd with
  | Nil, _ -> fd
  | _, Nil -> fg
  | Noeud(k1, gauche1, droit1), Noeud(k2, gauche2, droit2) -> Noeud(
      k1,
      gauche1,
      Noeud(
          k2,
          fusion droit1 gauche2,
          droit2
        )
    )
;;
(*Attention, on prend le risque d'augmenter la hauteur de l'arbre*)

let rec retrait_racine2 = function
  | Nil -> failwith "Pas de racine"
  | Noeud(_, Nil, droit) -> droit
  | Noeud(_, gauche, droit) -> let e_max, gauche_final = extract_max gauche in
    Noeud(e_max, gauche_final, droit)
and extract_max = function
  | Noeud(k, gauche, Nil) -> (k, gauche)
  | Noeud(k, gauche, droit) -> let e_max, droit_final = extract_max droit in
    (e_max, Noeud(k, gauche, droit_final))
  | Nil -> failwith "Arbre vide"
;;

let rec retrait_elem elem = function
  | Nil -> Nil
  | Noeud(k, gauche, droit) when elem > k -> Noeud(k, gauche, retrait_elem elem droit)
  | Noeud(k, gauche, droit) when elem < k -> Noeud(k, retrait_elem elem gauche, droit)
  | noeud -> retrait_racine2 noeud
;;
