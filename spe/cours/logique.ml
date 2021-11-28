(*Cours de logique*)

(*
Définition : On appelle variable propositionnelle toute variable pouvant prendre la valeur `Vrai` ou `Faux`

On appelle connecteur logique toute application de E**n dans E, où E = {Vrai, Faux}
On dit que le connecteur est d'arité n (ou est n-aire)

Rq: il y a 2 ** (2 ** n)

Il y a 4 opérateurs unaires, 16 opérateurs binaires

Proposition : {NON, ET, OU} forme un système complet de connecteurs logiques.
C'est-à-dire que toute formule propositionnelle peut être réécrite en une formule propositionnelle équivalente uniquement à l'aide de ces opérateurs.

Proposition : {NAND} forme un système complet de connecteurs logiques

Preuve :

NON f == f NAND f

f ou g = NON (NON f et NON g) = (NON F) NAND (NON g)

f ET g = NON (f NAND g)


Définition : On dit qu'une formule propositionnelle est sous forme normale conjonctive (FNC) lorsqu'elle est écrite
sous la forme : C1 ^ C2 ^ ... ^ Cn
avec Ci de la forme (li,1 v li,2 v ... v li,pi) et li,k est soit une variable, soit la négation d'une variable.

Ci porte alors le nom de clause, et li,p porte le nom de littéral.

Définition: On dit qu'une formule propositionnelle est sous forme normale disjonctive (FND) lorsqu'elle s'écrit sous la forme :

C1 v ... v Cn
avec Ci de la forme (li,1 ^ ... ^ li, pi), et li,r est un littéral

Problème SAT

Données : une formule propositionnelle
Résultat : la formule est-elle satisfiable ?

Ce problème est très célèbre dans la littérature car c'est un de ces fameux problèmes NP-complets
NP : On peut vérifier qu'une solution en est une en un temps polynomial.

SAT permet de résoudre tout problème dans NP

Problème 3-SAT :
Données : une formule propositionnelle sous forme normale conjonctive avec des clauses de 3 littéraux
Résultat f est-elle satisfiable ?
Ce problème est aussi NP-complet ?

Rg : le 2-SAT est dans P

Si l'on veut savoir si une formul eest satisfiable, il est intéressant de l'avoir sous forme normale disjonctive

Comment obtenir la forme normale disjonctive ?
On établi la table de vérité !

Comment obtenir la forme normale conjonctive  ?
On cherche la forme normale disjonctive de la négation et on en prend la négation


On représente en générale sous forme arborescente
*)

type formule =
| NON of formule
| ET of formule * formule
| OU of formule * formule
| Var of string
;;

let rec eval formule vars = match formule with
    | NON(f1) -> not (eval f1 vars)
    | ET(f1, f2) -> (eval f1 vars) && (eval f2 vars)
    | OU(f1, f2) -> (eval f1 vars) || (eval f2 vars)
    | Var(varname) -> List.mem varname vars
;;
