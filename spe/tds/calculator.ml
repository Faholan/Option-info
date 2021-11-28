(*
Calculator algorithm

Copyright (C) 2021  Faholan <https://github.com/Faholan>
*)

type expr =
  | Int of int
  | Plus of expr * expr
  | Minus of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
;;

(*Question 2*)

let rec feuille = function
  | Int(_) -> 1
  | Plus(e1, e2) -> (feuille e1) + (feuille e2)
  | Minus(e1, e2) -> (feuille e1) + (feuille e2)
  | Mult(e1, e2) -> (feuille e1) + (feuille e2)
  | Div(e1, e2) -> (feuille e1) + (feuille e2)
;;

(*Question 3*)

let rec compute = function
  | Int(x) -> x
  | Plus(e1, e2) -> (compute e1) + (compute e2)
  | Minus(e1, e2) -> (compute e1) - (compute e2)
  | Mult(e1, e2) -> (compute e1) * (compute e2)
  | Div(e1, e2) -> let divider = compute e2 in
    if divider = 0 then
      failwith "ZeroDivisionError"
    else
      (compute e1) / divider
;;

(*Question 4*)

let lookup index =
  let rec fetch = function
    | [] -> failwith "unbound variable"
    | (i, v)::_ when i = index -> v
    | _::t -> fetch t
  in fetch
;;

(*Question 5*)
type expr =
  | Int of int
  | Plus of expr * expr
  | Minus of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Var of string
;;

let compute vars =
  let rec computer = function
    | Int(x) -> x
    | Plus(e1, e2) -> (computer e1) + (computer e2)
    | Minus(e1, e2) -> (computer e1) - (computer e2)
    | Mult(e1, e2) -> (computer e1) * (computer e2)
    | Div(e1, e2) -> let divider = computer e2 in
      if divider = 0 then
        failwith "ZeroDivisionError"
      else
        (computer e1) / divider
    | Var(name) -> lookup name vars
  in computer
;;

(*Question 6*)
type expr =
  | Int of int
  | Plus of expr * expr
  | Minus of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Var of string
  | Let of string * expr * expr
;;

let rec compute vars = function
  | Int(x) -> x
  | Plus(e1, e2) -> (compute vars e1) + (compute vars e2)
  | Minus(e1, e2) -> (compute vars e1) - (compute vars e2)
  | Mult(e1, e2) -> (compute vars e1) * (compute vars e2)
  | Div(e1, e2) -> let divider = compute vars e2 in
    if divider = 0 then
      failwith "ZeroDivisionError"
    else
      (compute vars e1) / divider
  | Var(name) -> lookup name vars
  | Let(name, content, expr) -> compute ((name, compute vars content)::vars) expr
;;

(*Question 7*)
type expr =
  | Int of int
  | Plus of expr * expr
  | Minus of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Var of string
  | Let of string * expr * expr
  | If of cond * expr * expr
and cond =
  | Not of cond
  | And of cond * cond
  | Or of cond * cond
  | Equal of expr * expr
  | Less of expr * expr
;;

let rec compute vars = function
  | Int(x) -> x
  | Plus(e1, e2) -> (compute vars e1) + (compute vars e2)
  | Minus(e1, e2) -> (compute vars e1) - (compute vars e2)
  | Mult(e1, e2) -> (compute vars e1) * (compute vars e2)
  | Div(e1, e2) -> let divider = compute vars e2 in
    if divider = 0 then
      failwith "ZeroDivisionError"
    else
      (compute vars e1) / divider
  | Var(name) -> lookup name vars
  | Let(name, content, expr) -> compute ((name, compute vars content)::vars) expr
  | If(cond, e1, e2) -> if compute_bool vars cond then
    compute vars e1
  else
    compute vars e2
and compute_bool vars = function
  | Not(cond) -> not (compute_bool vars cond)
  | And(cond1, cond2) -> (compute_bool vars cond1) && (compute_bool vars cond2)
  | Or(cond1, cond2) -> (compute_bool vars cond1) || (compute_bool vars cond2)
  | Equal(e1, e2) -> (compute vars e1) = (compute vars e2)
  | Less(e1, e2) -> (compute vars e1) < (compute vars e2)
;;

(*Partie 2 - fonctions*)
type valeur =
  | Ival of int
  | SomethingElse
;;

(*Question 8*)

let rec compute vars = function
  | Int(x) -> x
  | Plus(e1, e2) -> (compute vars e1) + (compute vars e2)
  | Minus(e1, e2) -> (compute vars e1) - (compute vars e2)
  | Mult(e1, e2) -> (compute vars e1) * (compute vars e2)
  | Div(e1, e2) -> let divider = compute vars e2 in
    if divider = 0 then
      failwith "ZeroDivisionError"
    else
      (compute vars e1) / divider
  | Var(name) -> (match lookup name vars with
    | Ival(x) -> x
    | SomethingElse -> failwith "SomethingElse in an expression"
  )
  | Let(name, content, expr) -> compute ((name, Ival(compute vars content))::vars) expr
  | If(cond, e1, e2) -> if compute_bool vars cond then
    compute vars e1
  else
    compute vars e2
and compute_bool vars = function
  | Not(cond) -> not (compute_bool vars cond)
  | And(cond1, cond2) -> (compute_bool vars cond1) && (compute_bool vars cond2)
  | Or(cond1, cond2) -> (compute_bool vars cond1) || (compute_bool vars cond2)
  | Equal(e1, e2) -> (compute vars e1) = (compute vars e2)
  | Less(e1, e2) -> (compute vars e1) < (compute vars e2)
;;

(*Question 9*)

type valeur =
  | Ival of int
  | Fonction of string * expr * (string * valeur) list
and expr =
  | Int of int
  | Plus of expr * expr
  | Minus of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Var of string
  | Let of string * expr * expr
  | If of cond * expr * expr
  | Fun of string * expr
  | App of expr * expr
and cond =
  | Not of cond
  | And of cond * cond
  | Or of cond * cond
  | Equal of expr * expr
  | Less of expr * expr
;;

let rec compute vars expr = get_value (do_compute vars expr)
and do_compute vars = function
  | Int(x) -> Ival(x)
  | Plus(e1, e2) -> Ival((compute vars e1) + (compute vars e2))
  | Minus(e1, e2) -> Ival((compute vars e1) - (compute vars e2))
  | Mult(e1, e2) -> Ival((compute vars e1) * (compute vars e2))
  | Div(e1, e2) -> let divider = compute vars e2 in
    if divider = 0 then
      failwith "ZeroDivisionError"
    else
      Ival((compute vars e1) / divider)
  | Var(name) -> lookup name vars
  | Let(name, content, expr) -> do_compute ((name, do_compute vars content)::vars) expr
  | If(cond, e1, e2) -> if compute_bool vars cond then
    do_compute vars e1
  else
    do_compute vars e2
  | Fun(var, expr) -> Fonction(var, expr, vars)
  | App(func, content) -> execute vars func content
and compute_bool vars = function
  | Not(cond) -> not (compute_bool vars cond)
  | And(cond1, cond2) -> (compute_bool vars cond1) && (compute_bool vars cond2)
  | Or(cond1, cond2) -> (compute_bool vars cond1) || (compute_bool vars cond2)
  | Equal(e1, e2) -> (compute vars e1) = (compute vars e2)
  | Less(e1, e2) -> (compute vars e1) < (compute vars e2)
and get_value = function
  | Ival(x) -> x
  | Fonction(_, _, _) -> failwith "Function appeared in arithmetic expression"
and execute vars func content = match (do_compute vars func) with
  | Fonction(varname, expr, env) -> do_compute ((varname, do_compute vars content)::(env @ vars)) expr
  (*env@vars allows the function to defer to the environment of where it was called, enabling recursivity !*)
  | _ -> failwith "App only expects a function"
;;


let factorielle n =
  compute [] (Let(
    "fact",
    Fun(
      "n",
      If(
        Less(Var("n"), Int(2)),
        Int(1),
        Mult(
          Var("n"),
          App(
            Var("fact"),
            Minus(Var("n"), Int(1))
          )
        )
      )
    ),
    App(Var("fact"), Int(n))
  ))
;;

(*Ocaml equivalent :*)
(*
let factorielle n =
  let rec fact n =
    if n < 2 then
      1
    else
      n * (fact (n - 1))
  in fact n
;;
*)
