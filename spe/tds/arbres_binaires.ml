(*
TD sur les arbres binaires

Copyright (C) 2021  Faholan <https://github.com/Faholan>
*)

(*Partie I*)
type ('n, 'f) arbre_binaire =
    | Feuille of 'f
    | Noeud of 'n * ('n, 'f) arbre_binaire * ('n, 'f) arbre_binaire ;;

type squelette_binaire =
    | Rien
    | Jointure of squelette_binaire * squelette_binaire
;;

(*Question 1*)
(*a*)
let rec squelette = function
    | Feuille(_) -> Rien
    | Noeud(_, gauche, droite) -> Jointure(squelette gauche, squelette droite)
;;

(*b*)
let rec symetrique = function
    | Rien -> Rien
    | Jointure(gauche, droite) -> Jointure(symetrique droite, symetrique gauche)
;;

(*c*)
let rec squelette_min = function
    | 0 -> Rien
    | n -> Jointure(squelette_min (n - 1), Rien)
;;

(*d*)
let rec squelette_max = function
    | 0 -> Rien
    | n -> Jointure(squelette_max (n - 1), squelette_max (n - 1))
;;

(*Question 2*)

(*i*)
let rec complet1 arb =
    let result, _ = complet_profondeur arb in
        result
and complet_profondeur = function
    | Rien -> true, 0
    | Squelette(gauche, droite) -> let result_g, profondeur_g = complet_profondeur gauche in
        let result_d, profondeur_d = complet_profondeur droite in
            if gauche && droite then
                if profondeur_d = profondeur_g then
                    true, (profondeur_g + 1)
                else
                    false, 0
            else
                false, 0
;;
(*ii*)
let rec complet2 arb =
    let result, _, _ = complet_pmin_pmax arb in
        result
let complet_pmin_pmax arb =
    | Rien -> true, 0, 0
    | Squelette(gauche, droite) -> let rg, ming, maxg = complet_pmin_pmax gauche in
        let rd, mind, maxd = complet_pmin_pmax droit in
            if rg && rd then
                let minf = min ming mind and maxf = max maxg maxd in
                    if maxf - minf <= 1 then
                        true, minf + 1, maxf + 1
                    else
                        false, 0, 0
            else
                false, 0, 0
;;


(*Partie III*)

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

(*Question 1

On sait que dans un tas, la position du fils gauche de l'élément d'indice i est 2*i + 1
Celle de l'élément droit est 2*i + 2.
Celle du père est (i - 1) / 2

Ainsi un élément possède
- Un père ssi :
(i - 1) / 2 >= 0, soit i > 0
- Un fils gauche ssi :
2 * i + 1 < n
- Un fils droit ssi :
2 * i + 2 < n
*)

(*Question 2*)
let fils_g tas i =
    if 2 * i + 1 >= Array.length tas then
        failwith "Index out of bonds"
    else
        tas.(2 * i + 1)
;;


let fils_g tas i =
    if 2 * i + 2 >= Array.length tas then
        failwith "Index out of bonds"
    else
        tas.(2 * i + 2)
;;

let pere tas i =
    if i = 0 then
        failwith "Out of bonds"
    else
        tas.((i - 1) / 2)
;;

(*Question 3

La hauteur d'un tas est partie entière de log2(n)
*)

(*Question 4

Un tableau trié par ordre décroissant forme forcément un tas :
Chaque élément est plus grand que ceux d'indice supérieur.
Comme les fils d'un noeud sont d'indice supérieur, tout noeud est plus grand que chacun de ses fils.

Contre-exemple de la réciproque :

[|3; 1; 2|] est un tas mais le tableau n'est pas trié par ordre décroissant.
*)

(*Question 5
            23
        17          14
    6       13    10    1
5    7         12

Ce tableau n'est pas un tas, car l'élément d'indice 3 admet celui d'indice 8 comme fils, et 6 < 7.
*)

(*Question 6

Si le tas est de taille n, alors ses noeuds ont pour indice l'intervalle [2^(part_int log2(n)), n]
*)

(*3.2*)

(*Question 7*)

let fg i = 2 * i + 1 ;;
let fd i = 2 * i + 2 ;;

let echange tab i j =
    let val = tab.(i) in
        tab.(i) <- tab.(j) ;
        tab.(j) <- val
;;

let rec tamiser tas i =
    if fd i = Array.length tas then (
        if fils_d tas i > tas.(i) then
            echange tas i (fd i)
    ) else
        if fd i < Array.length tas then
            let pos_fmax = (if fils_d tas i > fils_g tas i then fd i else fg i) in
                if tas.(i) < tas.(pos_fmax) then (
                    echange tas i pos_fmax ;
                    tamiser tas pos_fmax
                )
;;
(*Question 8

A chaque appel, on fait au plus un appel récursif sur un fils du noeud traité.
Les opérations réalisées durant l'appel sont en O(1)
Tamiser est donc de complexité O(h), avec h la hauteur de l'arbre
*)

(*Question 9*)

let tasifier tas =
    for i = pere (Array.length tas - 1) downto 0 do
        tamiser tas i
    done
;;
(*
Le principe de tasifier est de transformer le tableau en tas.
Il suffit pour cela de tamiser les sous-arbres du futur tas, en commençant par ceux de hauteur 1,
Jusqu'à l'arbre complet. On a bien l'hypothèse que chaque sous-arbre est un tas.

La complexité est en O(n*log n) : On effectue n/2 appels environ à tamiser de complexité O(log n)
*)

let rec tamiser_bis tas i i_max =
    if fd i = i_max + 1 then (
        if fils_d tas i > tas.(i) then
            echange tas i (fd i)
    ) else
        if fd i <= i_max then
            let pos_fmax = (if fils_d tas i > fils_g tas i then fd i else fg i) in
                if tas.(i) < tas.(pos_fmax) then (
                    echange tas i pos_fmax ;
                    tamiser tas pos_fmax i_max
                )
;;

(*Question 12*)
let tri_par_tas tas =
    tasifier tas ;
    for i = Array.length tas - 2 downto 1 do
        echange tas 0 (i + 1) ;
        tamiser_bis tas 0 i
    done
;;

(*Question 13*)
(*La complexité de tasifier est en O(n*log n)
La complexité de tamiser_bis étant en O(log n), on obtient une complexité totale de :
O(n*log n)
*)

(*Question 14
Si le tableau est trié par ordre décroissant avant le début de la procédure, alors
chaque appel de tamiser ou tamiser_bis n'engendre aucun appel récursif.
La complexité de tamiser est donc en O(1), et celle du tri devient O(n)
*)

(*3.3*)
(*Question 15*)

let extraire_max tas n =
    echange tas 0 (n - 1) ;
    tamiser_bis tas 0 (n - 2) ;
    tas.(n - 1)
;;
(*Question 16*)

let pos_pere i = (i - 1) / 2 ;;

let rec inserer tas n elem =
    tas.(n) <- elem ;
    reparer tas n
and reparer tas = function
    | 0 -> ()
    | n when tas.(n) > pere tas n -> (
        echange tas n (pos_pere n) ;
        reparer tas (pos_pere n)
    )
    | _ -> ()
;;

(*Question 17
Le compromis choisi est probablement de créer un tableau initialement relativement petit,
et de l'agrandir à certains seuils : par exemple, doubler sa taille.

Cela permet d'amortir le coût de l'agrandissement sur un grand nombre d'insertion, et d'obtenir
un coût de l'insertion linéaire en moyenne.
*)

(*Partie 4*)

(*Question 8*)
type dictionnaire =
    | Null
    | End of dictionnaire
    | Letter of (char * dictionnaire * dictionnaire)
;;

(*a

Le principe du dictionnaire est que, pour chaque arbre, on a :
l'arbre droit ne commence pas par la lettre indiquée
*)

let string_tail mot =
    String.sub mot 1 (String.length mot - 1)
;;

let rec dicodumot mot = match String.length mot with
    | 0 -> End(Null)
    | n -> Letter(mot.[0], dicodumot (string_tail mot), Null)
;;

(*c*)
let rec ajouter mot = function
    | dc when mot = "" -> End(dc)
    | Null -> dicodumot mot
    | End(dc) -> End(ajouter mot dc)
    | Letter(l, left, right) when l = mot.[0] -> Letter(l, ajouter (string_tail mot) left, right)
    | Letter(l, left, right) -> Letter(l, left, ajouter mot right)
;;

(*d*)
let rec rechercher mot = function
    | End(_) when mot = "" -> true
    | Null -> false
    | Letter(l, left, _) when l = mot.[0] -> rechercher (string_tail mot) left
    | Letter(_, _, right) -> rechercher mot right
;;

(*e*)
let rec listing dc = listing_prefixed "" dc
and listing_prefixed prefix = function
    | Null -> []
    | End(dc) -> prefix::(listing_prefixed prefix dc)
    | Letter(l, left, right) -> (listing_prefixed ((String.make 1 l) ^ prefix) left) @ listing_prefixed prefix right
;;

(*f*)
let rec supprimer mot = function
    | End(dc) when mot = "" -> dc
    | Null -> Null
    | Letter(l, left, right) when l = mot.[0] -> Letter(l, supprimer (string_tail mot) left, right)
    | Letter(l, left, right) -> Letter(l, left, supprimer mot right)
;;
