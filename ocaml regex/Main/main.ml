open SimpleRegexp
open TestRe
open RePcre
open ReSimple
open ReKit

module TestsReSimple = TestRegexp.Test(ReSimple)
module TestsReKit = TestRegexp.Test(ReKit)
module TestsRePcre = TestRegexp.Test(RePcre)

let () =
let (i,f) = TestsReSimple.test () in
  (Printf.printf "ReSimple Test results:\n errors -- %d\n time -- %f\n" i f);
let (i,f) = TestsReKit.test () in
  (Printf.printf "ReKit Test results:\n errors -- %d\n time -- %f\n" i f);
let (i,f) = TestsRePcre.test () in
  (Printf.printf "RePcre Test results:\n errors -- %d\n time -- %f\n" i f);