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

namespace Aho_Corasick

  open System.IO
  open Common

  module Regression = begin

    let canonical (do_print: bool, fd: Option<StreamWriter>) =
      // Build canonical example
      
      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### Aho: Canonical"

      let dictionary = ["a"; "ab"; "bab"; "bc"; "bca"; "c"; "caa"]
      printer(sprintf "building structure for [%s]" <| String.concat ", " dictionary)
      ref_gen("## dictionary")
      dictionary |> Seq.iter (fun (s) -> ref_gen(s.Trim()))
      let example = Aho_Corasick.AC_Table<_>.Create (dictionary)
  
      printer(sprintf "Number of states: %i" example.Index.Count)
      ref_gen("## Number of states")
      ref_gen(sprintf "%i" example.Index.Count)
  
      ref_gen("## trie")
      for i in example.Index do
        let v = i.Value
        let nextStates = String.concat ", " (v.Children |> Seq.map (fun (i,_) -> string i))
        printer(sprintf "Prefix : %s; Suffix : %s; DictSuffix : %s; Labeled %b ;NextStates : %s" v.Prefix v.Suffix v.DictSuffix (Option.isSome v.Label) nextStates)
        [v.Prefix; v.Suffix; v.DictSuffix; sprintf "%b" (Option.isSome v.Label); nextStates] 
          |> Seq.iter(fun (s) -> if s.Trim().Length = 0 then () else ref_gen(s.Trim()))
  
      let tststring = "abccab"
      printer(sprintf "searching %s" tststring)
      ref_gen("## searching")
      for matches in Aho_Corasick.Utilities.find_substrings example tststring do
        let msg = sprintf "%A" matches
        printer(msg)
        ref_gen(msg)
  
    let dict_text (filename) (filter) (do_print: bool) (fd: Option<StreamWriter>) =
      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### Aho: dict text"

      let words = System.IO.File.ReadAllLines(filename) |> Array.filter (fun i -> i.Length > filter)
      let example = Aho_Corasick.AC_Table<_>.Create(words)
  
      let suffixes = example.Index |> Seq.countBy (fun i -> i.Value.Suffix.Length) |> Seq.sort |> Seq.map (fun (i,j) -> sprintf "(%i;%i)" i j) |> String.concat ", "
      let dictsuffixes = example.Index |> Seq.countBy (fun i -> i.Value.DictSuffix.Length ) |> Seq.sort |> Seq.map (fun (i,j) -> sprintf "(%i;%i)" i j) |> String.concat ", "
  
      let avgFanout = example.Index |> Seq.filter (fun i -> i.Value.Children.Count > 0) |> Seq.averageBy (fun i -> i.Value.Children.Count |> float)
  
      printer(sprintf "Number of states: %i; fanout: %1.4f suffixes: %s; dictsuffixes : %s number of words: %i; average states per words: %2.4f" example.Index.Count avgFanout suffixes dictsuffixes words.Length ((float example.Index.Count) / (float words.Length)))
      ref_gen("## dict info")
      [sprintf "%i" example.Index.Count; sprintf "%1.4f" avgFanout; suffixes; dictsuffixes; sprintf "%i" words.Length; 
      sprintf "%2.4f" ((float example.Index.Count) / (float words.Length))]  |> Seq.iter(fun (s) -> if s.Trim().Length = 0 then () else ref_gen(s.Trim()))

    let search_file (dictionary) (filename) (do_print: bool) (fd: Option<StreamWriter>) = 
      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### Aho: search file"

      let t = System.Diagnostics.Stopwatch()
      let buffer = System.IO.File.ReadAllBytes(filename)
      do t.Start()
      let words = System.IO.File.ReadAllLines(dictionary) |> Array.filter (fun i -> i.Length > 4)
      let example = Aho_Corasick.AC_Table<_>.Create(words)
      do t.Stop()
  
      let dictload = t.ElapsedMilliseconds
  
      do t.Restart()
      let matches = Aho_Corasick.Utilities.find_subsequence example buffer |> Seq.distinctBy fst |> Array.ofSeq
      do t.Stop()
  
      ref_gen "## matches"
      for i in matches do
        let msg = sprintf "%s" (fst i)
        printer(msg)
        ref_gen msg

      printer(sprintf "Dictionary load time: %ims ; scan time %ims; Number of matches : %i" dictload t.ElapsedMilliseconds matches.Length)
      ref_gen "## number of matches"
      ref_gen(sprintf "%i" matches.Length)
 
    // If fn is Some, reference values are printed to file
    // If do_print is true, then info is printed to screen
    let run_stage(do_print: bool, fd: Option<StreamWriter>, other_params: List<string>) =
      match other_params with
             | [] -> 
                 // Default
                 canonical(do_print, fd) 
                 let src = __SOURCE_DIRECTORY__
                 let dict = src + "/../dict1.txt"
                 let text = src + "/../text1.txt"
                 dict_text dict 0 do_print fd 
                 dict_text dict 5 do_print fd
                 search_file dict text do_print fd
                 true
             | ["dict"; dictionary; filter_val] when System.IO.File.Exists(dictionary) && let s,v = System.Int32.TryParse(filter_val) in s -> dict_text dictionary (System.Int32.Parse(filter_val)) do_print fd; true
             | ["search"; dictionary; target] when System.IO.File.Exists(dictionary) && System.IO.File.Exists(target) -> search_file dictionary target do_print fd; true
             | _ -> false
  end
  
