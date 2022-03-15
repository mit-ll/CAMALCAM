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

namespace DDFA

  open System.IO
  open Common

  module Regression = begin
    let basic (do_print: bool, fd: Option<StreamWriter>) =

      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### DDFA: Canonical"

      let example = 
        [|
          DDFA.DDFA.root<_,_,_>(None,0,[('a',1);('c',3)])
          DDFA.DDFA.defered<_,_,_>(None,0,[('b',2);('c',3)])
          DDFA.DDFA.defered<_,_,_>(None,0,[('c',3)])
          DDFA.DDFA.defered<_,_,_>(Some (),0,[])
        |]

      for line in DDFA.Utilities.Serializers.ToJson example do
        printer line
        ref_gen line

      printer(sprintf "Standard Aho-Corasick example")

      let dictionary = ["a"; "ab"; "bab"; "bc"; "bca"; "c"; "caa"]
  
      printer(sprintf "building structure for [%s]" <| String.concat ", " dictionary)  
      let example2 = Aho_Corasick.AC_Table<_>.Create (dictionary |> Seq.map (fun i -> (i,i))) |> DDFA.AhoCorasick.toDDFA

      printer(sprintf "Before chaining")
      ref_gen "## Before chaining"
      for line in DDFA.Utilities.Serializers.ToJson example2 do
        printer line
        ref_gen line

      let c = DDFA.Utilities.Optimzer.State_Expander.DeferChainReplacement (example2)
      printer(sprintf "After chaining %i times" c)
      ref_gen(sprintf "## After chaining")
      ref_gen(sprintf "%i" c)
      for line in DDFA.Utilities.Serializers.ToJson example2 do
        printer line
        ref_gen line

      let tststring = "abccab"
      printer(sprintf "searching %s" tststring)
      ref_gen("## searching")
      ref_gen tststring
      for matches in DDFA.Utilities.find_substrings example2 tststring do
        let msg = sprintf "%A" matches
        printer msg
        ref_gen msg

      for i = 2 to 3 do
        printer(sprintf "root stride : %i" i)
        ref_gen(sprintf "%i" i)
        let example3 = DDFA.MultiStride.k_stride_reduced(example2, i)
        for line in DDFA.Utilities.Serializers.ToJson example3 do
          printer line
          ref_gen line

        printer "Deferments"
        ref_gen "## deferments"
        let tree, roots = DDFA.Utilities.Deferment.get_forest example3
        for d in tree do
          let msg = sprintf "%i : [%s]" d.Key (d.Value |> Seq.map string |> String.concat ", ")
          printer msg
          ref_gen msg
        printer(sprintf "Roots : [%s]" (roots |> Seq.map string |> String.concat ", "))
        ref_gen(sprintf "%s" (roots |> Seq.map string |> String.concat ", "))

        printer(sprintf "searching %s" tststring)
        ref_gen "## searching"
        for matches in DDFA.MultiStride.find_substrings example3 tststring do
          printer(sprintf "%A" matches)
          ref_gen(sprintf "%A" matches)

    let dict_text (filename) (filter) (stride) (do_print: bool) (fd: Option<StreamWriter>) =
      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### DDFA: dict text"

      let words = System.IO.File.ReadAllLines(filename) |> Array.filter (fun i -> i.Length > filter) |> Seq.map (fun i -> (i,i))
      let example = Aho_Corasick.AC_Table<_>.Create(words) |> DDFA.AhoCorasick.toDDFA

      let example2 = DDFA.MultiStride.k_stride_reduced(example,stride)

      printer(sprintf "Single stride")
      ref_gen "## Single stride"
      for line in DDFA.Utilities.Serializers.ToJson example do
        printer line
        ref_gen line

      printer(sprintf "%i-stride" stride)
      ref_gen(sprintf "## %i-stride" stride)
      for line in DDFA.Utilities.Serializers.ToJson example2 do
        printer line
        ref_gen line

      //let reach = DDFA.MultiStride.compute_reachable(example2,0)

      //sprintf "Reachable states %s" (reach |> Seq.map string |> String.concat ", ") 
      printer(sprintf "%i out of %i are reachable" example2.Length example.Length)
      ref_gen(sprintf "%i" example2.Length)
      ref_gen(sprintf "%i" example.Length)
    
    let search_file (dictionary) (filename) (do_print: bool) (fd: Option<StreamWriter>) = 
      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### DDFA: search file"

      let t = System.Diagnostics.Stopwatch()
      let buffer = System.IO.File.ReadAllBytes(filename)
      do t.Start()
      let words = System.IO.File.ReadAllLines(dictionary) |> Array.filter (fun i -> i.Length > 4) |> Seq.map (fun i -> (i,i))
      let example = Aho_Corasick.AC_Table<_>.Create(words) |> DDFA.AhoCorasick.toDDFA
      //do example.[0] <- DDFA.Utilities.Optimzer.State_Expander.Expand example.[0]
      let mutable c = 0
      for i = 0 to example.Length - 1 do
        if DDFA.Utilities.Optimzer.State_Expander.DeferChainReplacement(example,i) then
          c <- c + 1
      
      let mutable e = 0
      for i = 0 to example.Length - 1 do
        if example.[i].Transitions.Count > 4 then
          e <- e + 1
          example.[i] <- DDFA.Utilities.Optimzer.State_Expander.Expand example.[i]
      do t.Stop()

      let dictload = t.ElapsedMilliseconds

      do t.Restart()
      let matches = DDFA.Utilities.find_subsequence example buffer |> Seq.countBy fst |> Seq.sortBy snd |> Array.ofSeq
      do t.Stop()

      for i,j in matches do
        printer(sprintf "%s : %i" i j)
        ref_gen i
        ref_gen(sprintf "%i" j)

      printer(sprintf "Dictionary load time: %ims (expanded %i states; chaining states %i) ; scan time %ims; Number of matches : %i" dictload e c t.ElapsedMilliseconds matches.Length)
      ref_gen(sprintf "%i" matches.Length)

    let search_file_stride (dictionary) (filename) (stride) (do_print: bool) (fd: Option<StreamWriter>) = 
      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### DDFA: search file stride"

      let t = System.Diagnostics.Stopwatch()
      let buffer = System.IO.File.ReadAllBytes(filename)
      do t.Start()
      let words = System.IO.File.ReadAllLines(dictionary) |> Array.filter (fun i -> i.Length > 4) |> Seq.map (fun i -> (i,i))
      let example' = Aho_Corasick.AC_Table<_>.Create(words) |> DDFA.AhoCorasick.toDDFA
      let example  = DDFA.MultiStride.k_stride_reduced(example',stride) 
      do t.Stop()

      let dictload = t.ElapsedMilliseconds

      do t.Restart()
      let matches = DDFA.MultiStride.find_subsequence example buffer |> Seq.countBy fst |> Seq.sortBy snd |> Array.ofSeq
      do t.Stop()

      for i,j in matches do
        printer(sprintf "%s : %i" i j)
        ref_gen i
        ref_gen(sprintf "%i" j)

      let originalStates = example'.Length
      let originalTransitions = example' |> Array.sumBy (fun i -> i.Transitions.Count)

      let newStates = example.Length
      let newTransitions = example |> Array.sumBy (fun i -> i.Transitions.Count)

      printer(sprintf "Dictionary load time: %ims ; scan time %ims; Number of matches : %i" dictload t.ElapsedMilliseconds matches.Length)
      ref_gen(sprintf "%i" matches.Length)
      printer(sprintf "Original States : %i Original Transitions : %i New States : %i New Transitions : %i" originalStates originalTransitions newStates newTransitions)
      [originalStates; originalTransitions; newStates; newTransitions] |> Seq.iter(fun(i) -> ref_gen(sprintf "%i" i))

    let run_stage(do_print: bool, fd: Option<StreamWriter>, other_params) =
         match other_params with
           | [] -> 
             // default
             basic(do_print, fd)
             let src = __SOURCE_DIRECTORY__
             let dict = src + "/../dict2.txt"
             let target = src + "/../text1.txt"
             dict_text dict 0 1 do_print fd 
             search_file_stride dict target 1 do_print fd
             true
           | ["dict"; dict; filter_val; stride_val] when System.IO.File.Exists(dict) && let stride,_ = System.Int32.TryParse(stride_val) in stride && let filter,_ = System.Int32.TryParse(filter_val) in filter ->             
            dict_text dict (System.Int32.Parse(filter_val)) (System.Int32.Parse(stride_val)) do_print fd; 
            true
           | ["search"; dict; target] when System.IO.File.Exists(dict) && System.IO.File.Exists(target) ->
            search_file dict target do_print fd
            true
           | ["search"; dict; stride_val; target] when System.IO.File.Exists(dict) && System.IO.File.Exists(target) && let stride,_ = System.Int32.TryParse(stride_val) in stride ->
            search_file_stride dict target (System.Int32.Parse(stride_val)) do_print fd
            true
           | _ -> false

  end