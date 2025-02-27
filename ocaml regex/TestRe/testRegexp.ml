open Regexp

module StringSet = Set.Make(String)

module Test(Re : REGEXP) = struct
  
  type lang = StringSet.t

  let rec pick_random_chars input acc n = 
    if n = 0 then acc else
      let idx = Random.int (String.length input) in
      let c = input.[idx] in
      pick_random_chars input (c :: acc) (n - 1)

  let random_word input n = 
    String.of_seq (List.to_seq (pick_random_chars input [] n))


  let make_lang str size = 
    let initial_set = ref StringSet.empty in
    for i = 1 to size do
      initial_set := StringSet.add (random_word str i) !initial_set
    done;
    !initial_set
      
  let select_accepted r l = 
    StringSet.filter (Re.matches r) l

  let test_two r1 r2 l b =
    let start_time = Sys.time () in
    let a_1 = select_accepted r1 l in
    let a_2 = select_accepted r2 l in
    let end_time = Sys.time () in
    let not_in_1 = StringSet.diff a_1 a_2 in
    let not_in_2 = StringSet.diff a_2 a_1 in
    let disjoint = (StringSet.union not_in_1 not_in_2) in
    if b then
      StringSet.iter (fun s -> print_endline s) disjoint;

    ((StringSet.cardinal disjoint), end_time -. start_time)

  let test_simple reg corr incorr = 
    let r = Re.re reg in
    let s_correct = StringSet.of_list corr in
    let s_incorrect = StringSet.of_list incorr in
    let start_time = Sys.time () in
    let accepted_1 = select_accepted r s_correct in
    let accepted_2 = select_accepted r s_incorrect in
    let end_time = Sys.time () in
    ((StringSet.cardinal accepted_2) 
    + (StringSet.cardinal s_correct) - (StringSet.cardinal accepted_1)      ,
      end_time -. start_time)

  let add_results res = 
    List.fold_left (fun (sum_int, sum_float) (i,f) -> (sum_int + i), (sum_float +. f))
    (0, 0.0) res

  let test () = 
    let results = [] in
    let results = results @ [(test_simple "ba*b" ["bb";"bab";"baaab";"b" ^ (String.make 100000 'a') ^ "b"] ["ba"; "babb";"baaaaaaaaaaaaaaabaaaaaaaaaaaaab";"aba"])] in
    let results = results @ [(test_simple "a*b" ["aaab"; "b"; "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"] ["a"; "aaaba"; "aaaaaaaaaabb"])] in
    let results = results @ [test_two (Re.re "a*a") (Re.re "aa*") (make_lang "ab" 100) true] in
    let results = results @
      [test_two (Re.re "(a|b)*(a|b)") (Re.re "(a|b)(a|b)*") (make_lang "abc" 100) true] in

    let results = results @ [test_two (Re.re "a(a|b)*|(a|b)*b") (Re.re "(a|b)*b|a(a|b)*") (make_lang "ab" 100) false] in

    (*Porównanie (a|b)* z ( a*b* )*   *)
    let results = results @ [test_two (Re.re "(a|b)*") (Re.re "(a*b*)*") (make_lang "abc" 100) false] in

    (*Porównanie a**** z a* *)
    let results = results @ [test_two (Re.re "a*") (Re.re "((a*)*)*") (make_lang "ab" 100) false] in
    add_results results

end
    


      