type t = {
  value : string;
  compiled : Pcre.regexp;
}

let re s = { value = s; compiled = Pcre.regexp ("^("^ s ^ ")$") }

let debug r = print_endline r.value

let matches r s =
  Pcre.pmatch ~rex:r.compiled s