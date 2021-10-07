(*
Cours sur les tas

Copyright (C) 2021  Faholan <https://github.com/Faholan>
*)

(*Définition

On appelle arbre binaire complet un arbre binaire dont tous les étages sont remplis.
*)

(*Proposition

Tout arbre binaire complet de hauteur h contient exactement 2 ^ (h + 1) - 1 éléments
*)

(*Définition

On appelle arbre binaire parfait tout arbre binaire dont tous les étages sont remplis
à l'exception peut-être du dernier.
Pour le dernier étage il faut de plus que tous ses éléments soient le plus à gauche.
*)

(*Proposition

Soit n le nombre d'éléments d'un arbre binaire parfait et h sa profondeur, alors :
2 ^ h <= n < 2 ^ (h + 1)

=> h = part_ent(log2 n)
*)

(*Problématique

Comment représenter un arbre binaire parfait à l'aide d'un tableau ?

On choisit de représenter l'arbre parfait comme le tableau représentant son parcours en largeur.

Si un élément est stocké en position i, son fils gauche est stocké en position (2i + 1)
et son fils droit en position (2i + 2).

Réciproquement, un noeud stocké à l'indice i, son père à l'indice int_sup(i/2) - 1 = part_int((i - 1)/2)

On adopte la représentation suivante :
*)

type 'a ap =
    {
        mutable libre: int ;
        tab: 'a array
    }
;;
(*Libre permet d'indiquer la première case libre du tableau.*)

let creer_ap n x =
    {
        libre = 0 ;
        tab = array.make n x
    }
;;

let fg i = 2 * i + 1 ;;

let fd i = 2 * i + 2 ;;

let pere i = (i - 1) / 2 ;;

(*
On crée une fonction de parcours d'un arbre.
A partir d'un chemin représenté par une liste de descente à gauche (0) ou à droite (1), déterminer à quel noeud on arrive
*)

let get_index =
    let rec get_from root = function
        | [] -> root
        | 0::t -> get_from (fg root) t
        | 1::t -> get_from (fd root) t
        | _::t -> failwith "Parcours invalide"
    in get_from 0
;;

(*Applications à la structure de tas*)

(*Définition

On dit qu'un arbre est un tas s'il est un arbre binaire parfait et une file de priorité.
*)

(*On choisit de représenter ce tas avec le tupe ap précedemment défini.*)

(*On implémente à présent les deux fonctions de manipulation d'une file de priorité*)

let element_prioritaire tas = match tas.libre with
    | 0 -> failwith "Le tas est vide"
    | _ -> tas.tab.(0)
;;

let echange tas i j =
    let elem = tas.tab.(i) in
        tas.tab.(i) <- tas.tab.(j) ;
        tas.tab.(j) <- elem
;;

let extrait_racine tas =
    if tas.libre = 0 then
        failwith "Le tas est vide"
    else (
        echange tas 0 (tas.libre - 1) ;
        tas.libre <- tas.libre - 1 ;
        reconstituer tas 0
    )
and reconstituer tas index =
    if fd index = tas.libre then (
        if tas.tab.(index) < tas.tab.(fg index) then
            echange tas index (fg index)
    ) else
        if fd index < tas.libre then
            let max_fils = (if tas.tab.(fg index) > tas.tab.(fd index) then fg index else fd index) in
                if tas.tab.(max_fils) > tas.tab.(index) then (
                    echange tas index max_fils ;
                    reconstituer tas max_fils
                )
;;

let inserer tas elem =
    if tas.libre = Array.length tas.tab then
        failwith "Taille maximale du tas excédée"
    else
        tas.tab.(tas.libre) <- elem ;
        tas.libre <- tas.libre + 1 ;
        reconstituer tas (tas.libre - 1)
and reconstituer tas index = match pere index with
    | -1 -> ()
    | n when tas.tab.(n) < tas.tab.(index) -> (
        echange tas n index ;
        reconstituer tas n
    )
    | _ -> ()
;;
