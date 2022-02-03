(*Théorie des langages et automates finis*)

(*
I) Langages : premières définitions
*)

(*
Définitions :

On appelle alphabet tout ensemble fini non vide noté Σ. Ses éléments sont appelés caractères.

On appelle mot toute suite finie de caractères, éventuellement vide.

La longueur d'un mot est le nombre de caractères qui le composent.

On note |ω| la longueur du mot ω

Le mot vide est la suite finie vide. On le note en général ε.

On appelle langage tout ensemble fini ou non de mots.

Définition : concaténation.

Soit ω1 = (a1, ..., an) et ω2 = (b1, ..., bp)

La concaténation de ωa et ωb est ωa.ωb = (a1, ..., an, b1, ..., bp)

Définition : opérations régulières (rationnelles)

Soit l1 et l2 deux langages sur un alphabet Σ.
On définit les 3 opérations rationnelles suivantes :

union : L1 U L2 l'ensemble des mots présents dans L1 ou L2

produit : L1 . L2 = {ω1.ω2 | ω1 € L1 et ω2 € L2}

Etoile de Kleene : L1* = U L1 ^ n

où L1 ^ n = {ε} si n = 0
L1 . L1 ^ (n - 1) si n >= 1

Remarques :

Σ^n est l'ensemble des mots de longueur n sur Σ
Σ* est l'ensemble de tous les mots que l'on peut créer sur Σ. Il contient tous les langages sur Σ.

Afin d'alléger les notations, les mots seront ainsi notés :
(ω1, ..., ωn) = ω1ω2...ωn

On définit ω^n = ε si n = 0, ω.ω^(n-1) si n >= 1

On identifie tout caractère au mot de longueur 1 composé par ce caractère.

Définition : Langages régulires (rationnels)

Définissons les langages rationnels par induction structurelle par induction :

∅, {ε} et tout singleton de Σ sont des langages rationnels.

L'union, le produit et l'étoile de Kleene de langages rationnels est un langage rationnel.

Autrement dit, l'ensemble des langages rationnels est stable par les opérateurs rationnels.


Définition : préfixe, facteur, suffixe.

Soit ω = pfs avec p, f, s € Σ*

On dit que :
  - p est un préfixe de ω
  - f est un facteur de ω
  - s est un suffixe de ω

Remarque : les préfixes et les suffixes de ω sont des facteurs de ω

II) Automates finis

Définition : on appelle automate fini déterministe tout quintuplet (Σ, Q, δ, i, F) où :
  - Σ est un alphabet
  - Q est un ensemble fini (non vide) qui est appelé ensemble des états de l'automate
  - i € Q est appelé état initial
  - F C Q appelé ensemble des états finaux
  - δ : I -> Q avec I C Q x Σ appelé fonction de transition
    -> Certaines transitions peuvent ne pas exister
    Si on arrive sur cela, on dit qu'on a un état de blocage. Le mot n'est pas accepté.
    Pour éviter les blocages, on peut compléter l'automate en ajoutant un état
    non finale nommé état puit ou poubelle vers lequel pointent toutes les transitions manquantes
    Lorsqu'un automate a toutes ses transitions, on parle d'automate complet.

Exemple : A = (Σ, Q, δ, i, F) avec :
  - Σ = {0, 1}
  - Q = {0, 1, 2}
  - i = 0
  - F = {0}
  - δ :
    | (0, 0) --> 0
    | (0, 1) --> 1
    | (1, 0) --> 1
    | (1, 1) --> 2
    | (2, 0) --> 2
    | (2, 1) --> 0

Représentation graphique d'un automate :
On représente les éléments q € Q par en l'entourant. On représente l'état initial avec une flèche allant vers lui.
On représente un état final comme doublement entouré

Définition : fonction de transition étendue.

On note δ* : Q x Σ* -> Q
  (q, ω1...ωn) -> q si n = 0, sinon δ*(δ(q, ω1), ω2...ωn)

Définitions :

On dit que ω € Σ* est accepté par l'automate fini déterministe A = (Σ, Q, δ, i, F) lorsque δ*(i, ω) € F
Dans le cas contraire le mot est refusé.

On définit le langage des mots acceptés par A comme étant l'ensemble des mots acceptés par A. On parle aussi de langage décrit par un automate.

Notation :
Lorsque δ(q1, c) = q2 on note q1 --> q2
                                  c
De même pour δ* avec une * sous la flèche

Remarque : un automate fini déterministe représente donc une nouvelle façon de décrire un langage.

Définition : Emonder un automate fini c'est retirer les états et les transitions associées à ces
états (et les transitions associées à ces états) qui ne contribuent pas à l'acceptation de mots dans
l'automate, c'est-à-dire qu'il n'exste aucun chemin allant de l'état initial à un état final passant par ces états.

Les états que l'on peut retirer se distinguent en 2 catégories :
  - Les états non accessibles : ce sont les états n'ayant pas de chemin allant vers eux depuis l'état initial.
  - LEs états non co-accessibles : ce sont les états n'ayant pas de chemin partant d'eux menant à un état final.


Les automates finis non déterministes

Définition : On appelle automate fini non déterministe tout quintuplet A = (Σ, Q, Δ, i, F) tel que :
  - Σ est un alphabet
  - Q est un ensemble non vide appelé ensemble des états
  - i € Q est l'état initial
  - F C Q est l'ensemble des états finaux
  - Δ C Q x Σ x Q est appelé ensemble des transitions

L'idée à présent est de s'autoriser d'avoir plusieurs transitions de la forme (q1, c, q2)

Définition :
  On dit que (q1, ..., qn) est un chemin d'un automate fini non déterministe lorsque :
  - ∀ i, qi € Q
  - ∀ i, ∃ ci € Σ, (qi, ci, qi+1) € Δ

Définition : un mot ω = ω1...ωn est accepté par un automate fini non déterministe
lorsqu'il existe un chemin menant de l'état initial à l'état final en lisant ce mot, ie :
∃ qf € F, i --> qf
            ω  *

Détermination d'un automate fini non déterministe
Pour déterminer un automate, on simule toutes les transitions de l'automate initial en essayant de savoir
après chaque transition, dans quels états l'automate de départ aurait pu se trouver

Rem : on n'obtiendra pas plus de 2 ** |Q| états après déterminisation de l'automate.
*)
