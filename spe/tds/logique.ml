type noeud =
    | Feuille of bool
    | Decision of string * int * int
;;

(*Question I.A*)
let monAD = [|
    Decision("e", 1, 2);
    Feuille(true);
    Decision("a", 4, 3);
    Feuille(false);
    Decision("r", 5, 6);
    Feuille(true);
    Feuille(false)
|]
;;


(*Question I.B*)
let rec eval_var varname = function
    | [] -> false
    | h::t -> (h = varname) || (eval_var varname t)
;;

(*Question I.C*)
let rec eval arb vars = eval_rec vars 0 arb
and eval_rec vars index arb = match arb.(index) with
    | Feuille(result) -> result
    | Decision(varname, i1, i2) -> eval_rec vars (if (eval_var varname vars) then i1 else i2) arb
;;

(*Question II.A*)
type noeud =
    | Feuille of bool
    | Decision of string * int * int
    | Vide
;;

let rec redirige diag v w =
    diag.(v) <- Vide ;
    for i = 0 to (Array.length diag - 1) do
        diag.(i) <- do_replace v w diag.(i)
    done ;
and do_replace v w = function
    | Decision(varname, i1, i2) when i1 = v && i2 = v -> Decision(varname, w, w)
    | Decision(varname, i1, i2) when i1 = v -> Decision(varname, w, i2)
    | Decision(varname, i1, i2) when i2 = v -> Decision(varname, i1, w)
    | whatever -> whatever
;;

(*Question II.B*)
let rec trouve_elimination diag = trouve_rec_eli diag 0
and trouve_rec_eli diag pos =
    if pos = Array.length diag then
        -1
    else (
        if sons_equal diag.(pos) then
            pos
        else
            trouve_rec_eli diag (pos + 1)
    )
and sons_equal = function
    | Decision(_, i1, i2) when i1 = i2 -> true
    | _ -> false
;;

(*Question II.C*)
let rec trouve_isomorphisme diag = trouve_rec_iso diag 0 1
and trouve_rec_iso diag pos1 pos2 =
    if pos1 = Array.length diag then
        -1, -1
    else (
        if pos2 = Array.length diag then
            trouve_rec_iso diag (pos1 + 1) (pos1 + 2)
        else (
            if equal_iso diag.(pos1) diag.(pos2) then
                pos1, pos2
            else
                trouve_rec_iso diag pos1 (pos2 + 1)
        )
    )
and equal_iso = function
    | Vide -> (fun _ -> false)
    | Decision(varname, i1, i2) -> equal_decis varname i1 i2
    | Feuille(content) -> equal_leaf content
and equal_decis var1 i1 i2 = function
    | Decision(var2, j1, j2) -> var1 = var2 && i1 = j1 && i2 = j2
    | _ -> false
and equal_leaf v1 = function
    | Feuille(v2) -> v1 = v2
    | _ -> false
;;

(*Question II.D*)

(*Question II.E*)
let rec reduit diag =
    let pos_eli = trouve_elimination diag in
        if pos_eli <> -1 then (
            redirige diag pos_eli (getson diag.(pos_eli)) ;
            reduit diag
        ) else (
            let iso1, iso2 = trouve_isomorphisme diag in
                if iso1 <> -1 then (
                    redirige diag iso1 iso2 ;
                    reduit diag
                )
        )
and getson = function
    | Decision(_, i1, i2) -> i1
    | _ -> failwith "Nah"
;;

(*Question III.A*)
(*
NON x = (x ^ FAUX) v (NON x ^ VRAI)

NON x = x->0,1

x OU y = NON (NON x ET NON y)

x OU y = ((x->0,1)->(y->0,1),0)->0,1

x ET y = x->y,0
*)

(*Question III.B*)
(*
(a->b,c)->d,e = (
    (a->b,c)^d
)v(
    ¬(a->b,c)^e
)

= (
    (
        (a^b)v(¬a^c)
    )^d
)v(
    ¬(
        (a^b)v(¬a^c)
    )^e
)

= (
    (
        (a^b)v(¬a^c)
    )^d
)v(
    (
        ¬(a^b)^¬(¬a^c)
    )^e
)

= (
    (
        (a^b)v(¬a^c)
    )^d
)v(
    (
        (¬av¬b)^(av¬c)
    )^e
)

= (
    (
        (a^b)v(¬a^c)
    )^d
)v(
    (
        (¬a^(¬c))v(¬b^a)
    )^e
)

= (
    (a^b^d)v(¬a^c^d)
)v(
    (¬a^(¬c)^e)v(¬b^a^e)
)

= (
    (a^b^d)v(a^¬b^e)
)v(
    (¬a^c^d)v(¬a^(¬c)^e)
)

= (
    a^(
        (b^d)v(¬b^e)
    )
)v(
    ¬a^(
        (c^d)v(¬c^e)
    )
)

= a->(b->d,e),(c->d,e)
*)

(*Question III.C
{NON, ET, OU} forme un système complet

Donc {.-->.,.} est un système complet.

Donc toute formule logique peut s'écrire à l'aide uniquement de l'opérateur -->

De plus, l'opérateur --> peut se factoriser comme vu en III.B

A tout étape de construction de l'arbre, on a une formule sous la forme a->(b,c), avec a une variable.

On a donc le noeud (a, arbre(b), arbre(c))
*)


(*Réaliser un additionneur bit à bit*)
(*
Commençons par un additionneur de 1 bit

a0  - ADD - s0
b0  -     - r0

a0² + b0² = r0s0²

*)
