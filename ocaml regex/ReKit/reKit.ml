open Regextkit
type t = {value : string; compiled: Tree.re}


let replace str original_char to_replace_char =
  String.map (fun c -> if c = original_char then to_replace_char else c) str

let re s = {value = s; compiled = Re.parse (replace (replace s '|' '+') '\n' '\t' )}

let debug r = print_endline r.value

let matches r s =
  let v = List.of_seq (String.to_seq s) in
  Re.is_nullable (List.fold_left (fun acc x -> Re.simplify (Re.derivative acc (String.make 1 x))) r.compiled v)
