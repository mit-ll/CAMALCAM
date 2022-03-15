(*
DISTRIBUTION STATEMENT A. Approved for public release. Distribution is unlimited.

This material is based upon work supported by the Under Secretary of Defense for
Research and Engineering under Air Force Contract No. FA8702-15-D-0001. Any
opinions, findings, conclusions or recommendations expressed in this material
are those of the author(s) and do not necessarily reflect the views of the Under
Secretary of Defense for Research and Engineering.

© 2022 Massachusetts Institute of Technology.

The software/firmware is provided to you on an As-Is basis

Delivered to the U.S. Government with Unlimited Rights, as defined in DFARS Part
252.227-7013 or 7014 (Feb 2014). Notwithstanding any copyright notice, U.S.
Government rights in this work are defined by DFARS 252.227-7013 or DFARS
252.227-7014 as detailed above. Use of this work other than as specifically
authorized by the U.S. Government may violate any copyrights that exist in this
work.
*)

namespace FDD

  open System.IO
  open Common
  open FDD.Sets

  module Regression = begin

    let members (a : BitSet) = 
      sprintf "{%s}" <| String.concat ", " (a.Members() |> Seq.map string)

    let test_bitset(do_print: bool, fd: Option<StreamWriter>) =
  
      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### FDD: test bitset"

      let test (a : BitSet) (b:BitSet) = 
        printer(sprintf "%A = %A : %b" (a.get_array()) (b.get_array()) (a=b))
        ref_gen(sprintf "%A" (a.get_array()))
        ref_gen(sprintf "%A" (b.get_array()))
        ref_gen(sprintf "%b" (a = b))

      let a = System.Collections.BitArray([|8uy|])
      let b = System.Collections.BitArray([|8uy|])
      let c = System.Collections.BitArray([|7uy|])
  
      let a' = BitSet(a) 
      let b' = BitSet(b) 
      let c' = BitSet(c) 
      let d' = a'.Union(c') 
      let e' = a'.Intersect(c')
      let f' = a'.Inverse()
  
      printer(sprintf "a' = %s" <| members a')
      printer(sprintf "b' = %s" <| members b')
      printer(sprintf "c' = %s" <| members c')
      printer(sprintf "d' = %s" <| members d')
      printer(sprintf "e' = %s" <| members e')
      printer(sprintf "f' = %s" <| members f')
      ref_gen(members a')
      ref_gen(members b')
      ref_gen(members c')
      ref_gen(members d')
      ref_gen(members e')
      ref_gen(members f')

      test a' b'
      test a' c'
      test a' d'
      test a' e'
      test a' f'

    let test_byteint_rule (do_print: bool, fd: Option<StreamWriter>) =
      
      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### FDD: test byteint rule"

      let test (a,b) (r : FDD.Rules.Interval<_,_>) =
        ref_gen(sprintf "%s" (string a))
        ref_gen(sprintf "%s" (string b))
        ref_gen(sprintf "%s" (string r))
        ref_gen(sprintf "%b" (r.Test(0,a) && r.Test(1,b)))
        printer(sprintf "(%s,%s) in %s : %b" (string a) (string b) (string r) (r.Test(0,a) && r.Test(1,b)))

      let r1 = FDD.Rules.Interval.Make([|(2uy,121uy);(5uy,70uy)|],"foo")
      let r2 = FDD.Rules.Interval.Make([|(2uy,121uy);(5uy,70uy)|],"foo", [|1;0|])

      test (3uy,6uy) r1
      test (3uy,6uy) r2

    let brules = [|
      FDD.Rules.Interval.Make([|(2uy,121uy);(5uy,70uy)|],"foo")
      FDD.Rules.Interval.Make([|(5uy,70uy);(5uy,70uy)|],"foo")
      FDD.Rules.Interval.Make([|(2uy,121uy);(2uy,121uy)|],"foo")
      FDD.Rules.Interval.Make([|(20uy,40uy);(20uy,40uy)|],"foo")
      FDD.Rules.Interval.Make([|(0uy,255uy);(0uy,255uy)|],"bar")
    |]

    let test_fdd_projections (do_print: bool, fd: Option<StreamWriter>) =

      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### FDD: test fdd projections"

      let proj = FDD.Byte_FDD.projectionArray BitSet.Empty (Seq.ofArray brules)

      brules |> Array.iteri (fun i j -> printer(sprintf "%i : %s" i (string j)); ref_gen(sprintf "%i %s" i (string j)))

      (proj.[0], proj.[1])
      ||> Seq.zip
      |>  Seq.iteri
            (fun idx (i,j) ->
              printer(sprintf "%i : %s : %s" idx (members i) (members j))
              ref_gen(sprintf "%i %s %s" idx (members i) (members j))
            )

    let test_fdd_build (do_print: bool, fd: Option<StreamWriter>) =

      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### FDD: test fdd build"

      let printer = print_or_write fd

      let toDecision (i : BitSet) = i.Members() |> Seq.map string |> String.concat ", "  

      let fdd = FDD.Byte_FDD.ofRules toDecision BitSet.Empty (Seq.ofArray brules)

      printer(sprintf "Number of Nodes : %i" fdd.NodeCount)
      ref_gen(sprintf "%i" fdd.NodeCount)
  
      for nodes in fdd.Representation(string,id) do
        match nodes with
        | FDD.FDD_Representation.Node (id, ds) -> ds |> Seq.distinctBy snd |> Seq.map (fun (a,b) -> sprintf "%s -> %i" a b) |> String.concat "; " |> sprintf "node : %i; to = [%s]" id |> printer
        | FDD.FDD_Representation.Terminal (id, d) -> printer(sprintf "term : %i; d=%s" id d); ref_gen(sprintf "%i %s" id d)

      printer(sprintf "%s" (fdd.ToString()))
      ref_gen(fdd.ToString())

    let run_stage(do_print: bool, fd: Option<StreamWriter>, other_params) =
         match other_params with 
         | [] ->
           // default tests
           test_bitset(do_print, fd)
           test_byteint_rule(do_print, fd)
           test_fdd_projections(do_print, fd)
           test_fdd_build(do_print, fd)
           true
         | ["bitset"] -> test_bitset(do_print, fd); true
         | ["byterule"] -> test_byteint_rule(do_print, fd); true
         | ["fddproj"] -> test_fdd_projections(do_print, fd); true
         | ["fddbuild"] -> test_fdd_build(do_print, fd); true
         | _ -> false

   end