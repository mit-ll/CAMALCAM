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

namespace TCAM

  open System
  open System.IO
  open Common

  module Regression = begin

    let basic (do_print: bool, fd: Option<StreamWriter>) = 
      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### TCAM: basic"

      let dictionary = ["a"; "ab"; "bab"; "bc"; "bca"; "c"; "caa"]
  
      printer(sprintf "building structure for [%s]" <| String.concat ", " dictionary)
      ref_gen(sprintf "%s" <| String.concat ", " dictionary)
      let example =  Aho_Corasick.AC_Table<_>.Create (dictionary |> Seq.map (fun i -> (i,i))) |> DDFA.AhoCorasick.toDDFA
      let replacements = DDFA.Utilities.Optimzer.State_Expander.DeferChainReplacement (example)


      for i in DDFA.Utilities.Serializers.ToJson example do
        printer(sprintf "%s" i)
        ref_gen i

      printer(sprintf "defer tree")
      ref_gen "## defer tree"
      let tree, root = DDFA.Utilities.Deferment.get_forest example
      for i in tree do
        printer(sprintf "%i => [%s]" i.Key (i.Value |> Seq.map string |> String.concat ", "))
        ref_gen(sprintf "%i %s "i.Key (i.Value |> Seq.map string |> String.concat ", "))

      printer(sprintf "shadow codes")
      ref_gen "## shadow codes"
      for (id,code,encoding) in TCAM.Utilities.ShadowEncoder.encode_tree tree (root.[0]) example.Length do
        printer(sprintf "%04i : %s : %s" id code encoding)
        ref_gen(sprintf "%04i %s %s" id code encoding)

      let ((startId,startMask),tcam) = TCAM.Utilities.TCAMConvert.toTCAM (example, 3)

      printer(sprintf "StartID : %A ; StartMask : %A" startId startMask)
      ref_gen(sprintf "%A %A" startId startMask)
      for r in tcam.Rows do
        printer(sprintf "%s => %A" (r.BankString()) r.decision)
        ref_gen(sprintf "%s %A" (r.BankString()) r.decision)
  
    let gen_cam (dictionary) (filter) (stride) (do_print: bool) (fd: Option<StreamWriter>) = 

      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### TCAM: gen_cam"

      let example =  Aho_Corasick.AC_Table<_>.Create (System.IO.File.ReadAllLines(dictionary) |> Seq.filter (fun i -> i.Length > filter) |> Seq.map (fun i -> (i,i))) |> DDFA.AhoCorasick.toDDFA
      let replacements = DDFA.Utilities.Optimzer.State_Expander.DeferChainReplacement (example)

      let ((startId,startMask),tcam) = TCAM.Utilities.TCAMConvert.toTCAM (example, stride)

      let tst = tcam.Rows |> Seq.head
      let stateBanks = tst.bank.Length - stride
      let headers = [|
        for idx = 0 to stateBanks - 1 do yield sprintf "state %i" idx
        for idx = 0 to stride - 1 do yield sprintf "Data %i" idx
    
        yield "match"
        yield "advance"
        for idx = 0 to stateBanks - 1 do yield sprintf "nextState %i" idx
      |]
      let msg = sprintf "%s" (headers |> String.concat ",") 
      printer msg
      ref_gen msg
      // TODO make this test result less wordy
      for r in tcam.Rows do
        let s = seq{
          for b in r.bank do yield b.ToString()
          let (m,a,n) = r.decision
          yield match m with Some x -> x | None -> ""
          yield a.ToString()
          for b in n do yield b.ToString()
        } 
        s |> String.concat "," |> sprintf "%s" |> printer
        s |> String.concat "," |> sprintf "%s" |> ref_gen

      printer(sprintf "Start ID : [%s], StartMask : [%s]" (startId |> Seq.map string |> String.concat ",") (startMask |> Seq.map string |> String.concat ","))
      ref_gen(sprintf "%s %s"  (startId |> Seq.map string |> String.concat ",") (startMask |> Seq.map string |> String.concat ","))

    let basic_tcam (do_print: bool, fd: Option<StreamWriter>) = 
      // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### TCAM: basic_tcam"

      let tst (name : string) (x : TCAM_Row<_>) = 
        let res = x.Test(0) |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "%s : [%s]" name
        res |> printer
        res |> ref_gen

      let rule1 = {bank=[|TCAM_Byte("10101010")|]; decision =()}
      let rule2 = {bank=[|TCAM_Byte("1*1**0**")|]; decision =()}

      tst "r1" rule1
      tst "r2" rule2

    let gen_cam_fdd (dictionary) (filter) (stride) (do_print: bool) (fd: Option<StreamWriter>) =  

       // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### TCAM: gen_cam"

      let example =  Aho_Corasick.AC_Table<_>.Create (System.IO.File.ReadAllLines(dictionary) |> Seq.filter (fun i -> i.Length > filter) |> Seq.map (fun i -> (i,i))) |> DDFA.AhoCorasick.toDDFA
      let replacements = DDFA.Utilities.Optimzer.State_Expander.DeferChainReplacement (example)

      let ((startId,startMask),tcam) = TCAM.Utilities.TCAMConvert.toTCAM (example, stride)

      let fdd = 
        FDD.Byte_FDD.ofRules 
          (fun (i : FDD.Sets.BitSet) -> tcam.Rows.[i.Members() |> Seq.head].decision) 
          FDD.Sets.BitSet.Empty 
          (tcam.Rows)

      printer(sprintf "Number of Nodes : %i" fdd.NodeCount)
      ref_gen(sprintf "%i" fdd.NodeCount)
  
      for nodes in fdd.Representation(string,string) do
        match nodes with
        | FDD.FDD_Representation.Node (id, ds) ->
          let s =  ds |> Seq.distinctBy snd |> Seq.map (fun (a,b) -> sprintf "%s -> %i" a b) |> String.concat "; " |> sprintf "node : %i; to = [%s]" id
          s |> printer
          s |> ref_gen
        | FDD.FDD_Representation.Terminal (id, d) -> printer(sprintf "term : %i; d=%s" id d); ref_gen(sprintf "%i %s" id d)

    let sim_cam_fdd (dictionary) (filter) (stride) (filename) (do_print: bool) (fd: Option<StreamWriter>) =

        // setup 
      let ref_gen = maybe_write fd
      let printer = maybe_print do_print
      ref_gen "#### TCAM: gen_cam"

      let t = new System.Diagnostics.Stopwatch()

      do t.Start()
      let example =  Aho_Corasick.AC_Table<_>.Create (System.IO.File.ReadAllLines(dictionary) |> Seq.filter (fun i -> i.Length > filter) |> Seq.map (fun i -> (i,i))) |> DDFA.AhoCorasick.toDDFA
      let replacements = DDFA.Utilities.Optimzer.State_Expander.DeferChainReplacement (example)
      do t.Stop()

      let build_dfa_ms = t.ElapsedMilliseconds

      do t.Restart()
      let ((startId,startMask),tcam) = TCAM.Utilities.TCAMConvert.toTCAM (example, stride)
      do t.Stop()
      let build_tcam_bank_ms = t.ElapsedMilliseconds

      do t.Restart()
      let fdd = 
        FDD.Byte_FDD.ofRules 
          (fun (i : FDD.Sets.BitSet) -> tcam.Rows.[i.Members() |> Seq.head].decision) 
          FDD.Sets.BitSet.Empty 
          (tcam.Rows)
      do t.Stop()

      let build_fdd_ms = t.ElapsedMilliseconds
      let tcam_width   = tcam.Rows.[0].bank.Length

      printer(sprintf "Time DDFA: %ims ; TCAM Size : %i rows; TCAM build time: %ims; Number of FDD Nodes : %i;  FDD build time %ims" 
        build_dfa_ms tcam.Rows.Count build_tcam_bank_ms fdd.NodeCount build_fdd_ms)
      ref_gen(sprintf "%i %i" tcam.Rows.Count fdd.NodeCount)

      let stateID = [|for i in startId do yield i.LiteralByte|]
      let data = System.IO.File.ReadAllBytes(filename)
      let buffered_data = Array.concat [stateID; data; [|for i = 1 to tcam_width - stateID.Length do yield 0uy |] ]

      let matches = new System.Collections.Generic.Dictionary<string,int>(HashIdentity.Structural)
      let haveMatch s = 
        let v = 
          let s,v = matches.TryGetValue(s)
          if s then v + 1 else 1
        matches.[s] <- v

      let data_span = buffered_data.AsSpan() 

      do t.Restart()

      let mutable cycles = 0
      let mutable idx = 0
      while idx + tcam_width < buffered_data.Length do
        // write state ID at idx
        for i = 0 to stateID.Length - 1 do data_span.[idx + i] <- stateID.[i] 
        // setup cursor
        let cursor : System.ReadOnlySpan<byte> = System.ReadOnlySpan( data_span.Slice(idx,tcam_width).ToArray() )
        // check fdd
        let strMatch, advance, nextState = FDD.Byte_FDD.getDecision (fdd, cursor)
        // did we match a string?
        match strMatch with | Some str -> haveMatch str | _ -> ()
        // copy state ID
        for i = 0 to stateID.Length - 1 do stateID.[i] <- nextState.[i].LiteralByte
        // advance cursor idx & count cycles
        do idx <- idx + advance
        do cycles <- cycles + 1

      do t.Stop()
      let simulate_in_ms = t.ElapsedMilliseconds

      for i in matches |> Seq.sortByDescending (fun i -> i.Value) do
        printer(sprintf "%s : %i" i.Key i.Value)

      printer(sprintf "simulation time %i; file size %i; number of cyles : %i; Average stride : %2.4f; bytes per ms (simulator): %2.4f"
        simulate_in_ms data.Length cycles ((float data.Length) / (float cycles)) ((float data.Length) / (float simulate_in_ms)))

    let run_stage(do_print: bool, fd: Option<StreamWriter>, other_params) = 
      match other_params with 
      | [] -> 
        // default
        basic(do_print, fd)
        basic_tcam(do_print, fd)
        let src = __SOURCE_DIRECTORY__
        let dict = src + "/../dict2.txt"
        let target = src + "/../text1.txt"
        gen_cam_fdd dict 0 5 do_print fd
        sim_cam_fdd dict 0 5 target do_print fd
        gen_cam dict 0 5 do_print fd
        true
      | ["unittcam"] -> basic_tcam(do_print, fd) ; true
      | ["fdd";dictionary;stride] when System.IO.File.Exists(dictionary) && let s,v = System.Int32.TryParse(stride) in s-> gen_cam_fdd dictionary 0 (System.Int32.Parse(stride)) do_print fd; true 
      | ["simulate";dictionary;stride;filename] when System.IO.File.Exists(filename) && System.IO.File.Exists(dictionary) && let s,v = System.Int32.TryParse(stride) in s-> sim_cam_fdd dictionary 0 (System.Int32.Parse(stride)) filename do_print fd; true
      | ["tcam";dictionary;stride] when System.IO.File.Exists(dictionary) && let s,v = System.Int32.TryParse(stride) in s-> gen_cam dictionary 0 (System.Int32.Parse(stride)) do_print fd; true
      | _ -> false

  end