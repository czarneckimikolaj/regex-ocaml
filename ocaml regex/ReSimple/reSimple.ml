open SimpleRegexp

type t = char reg

let re s = parse s

let debug r = print_endline (to_string r)

let matches r s = 
  accepts r s
