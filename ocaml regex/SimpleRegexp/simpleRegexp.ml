open SimpleRegexpDef
(* open Parser
open Lexer *)

type 'c reg = 'c SimpleRegexpDef.reg
(* type 'c reg =
  | Lit of 'c
  | Concat of 'c reg * 'c reg
  | Or of 'c reg * 'c reg
  | Star of 'c reg
  | Eps
  | Empty *)

let rec simpl r = match r with
| Concat (r1, Eps) -> simpl r1
| Concat (Eps, r2) -> simpl r2
| Concat (_, Empty) -> Empty
| Concat (Empty, _) -> Empty
| Concat (Concat (r1, r2), r3) -> Concat (simpl r1, Concat (simpl r2, simpl r3))
| Concat (r1, r2) -> Concat (simpl r1, simpl r2)
| Or (r1, Empty) -> simpl r1
| Or (Empty, r2) -> simpl r2
| Or (Eps, Eps) -> Eps
| Or (r1, r2) -> Or (simpl r1, simpl r2)
| Star (Star (r1)) -> Star (simpl r1)
| Star (Eps) -> Eps
| Star (Empty) -> Eps
| Star (r1) -> Star (simpl r1)
| r -> r


let empty r = match r with
| Empty -> true
| _ -> false

let rec nullable  r = match r with
| Eps -> true
| Or (Eps, _) -> true
| Or (_, Eps) -> true
| Or (r1, r2) -> nullable r1 || nullable r2
| Star (_) -> true
| Concat(r1, r2) -> nullable r1 && nullable r2
| _ -> false

let rec der c r = match r with
| Lit (d) when d = c -> Eps
| Lit (_) -> Empty
| Concat (r1, r2) when nullable r1 -> Or (Concat ((der c r1), r2), (der c r2))
| Concat (r1, r2) -> Concat ((der c r1), r2)
| Or (r1, r2) -> Or (der c r1, der c r2)
| Star (r) -> Concat ((der c r), Star(r))
| Eps -> Empty
| Empty -> Empty

let ders v r = List.fold_left (fun acc x -> simpl (der x acc)) r v

let accept r v = 
  nullable (ders v r)

let rec repr map r = match r with
| Lit (c) -> map c
| Eps -> "ε"
| Empty -> "∅"
| Or (r1, r2) -> "(" ^ (repr map r1) ^ "|" ^ (repr map r2) ^ ")"
| Concat (r1, r2) -> "("^(repr map r1) ^ (repr map r2) ^")"
| Star (r) -> (repr map r) ^ "*"

let char c = Lit (c)

let string s = String.fold_right (fun c acc -> Concat(Lit(c), acc)) s Eps

let alts s = String.fold_right (fun c acc -> Or(Lit(c), acc)) s Empty

let accepts r s =
  let string_list = List.of_seq (String.to_seq s) in
  accept r string_list

let rec to_string r = match r with
| Lit c -> String.make 1 c
| Star (r) -> to_string r ^ "*"
| Concat (r1, r2) -> "("^(to_string r1) ^ (to_string r2) ^")"
| Or (r1, r2) -> "(" ^ (to_string r1) ^ " | " ^ (to_string r2) ^ ")"
| Eps -> "ε"
| Empty -> "∅"

let parse s = Parser.regex Lexer.token (Lexing.from_string s)