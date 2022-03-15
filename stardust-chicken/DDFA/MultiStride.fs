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

  type MultiStride() = class
    static let find testat streamend (dfa : DDFA<int,_,string>[]) (s : 'a) =
      seq{
        let mutable state = dfa.[0] 
        let e = streamend s
        let mutable i = 0
        let mutable adv = 1
        while i <= e do
          let mutable repeat = true
          let test : string -> bool = testat s i
          let inline predicate (c,next) = if test c then Some ((c.Length), next) else None
          while repeat do
            match state.Transitions |> Seq.tryPick predicate with
            | Some (j,nxt) -> 
              adv <- j
              state <- dfa.[nxt]
              repeat  <- false
            | None  ->
              match state.Defer with
              | Deferment nxt -> 
                state <- dfa.[nxt]
                repeat <- true
              | DefaultTransition nxt ->
                state <- dfa.[nxt]
                repeat <- false
          // end do check for matches
          match state.Label with
          | Some x -> yield (x,i+(adv-1))
          | None -> ()
          i <- i + adv
          adv <- 1
        // end while
      }

    static member find_substrings (dfa : DDFA<int,_,string>[]) (s : string) =
      let substring_comp (s : string) (at : int) (tst : string) = 
        if at + tst.Length <= s.Length then
          let mutable c = at
          let mutable i = 0
          while i < tst.Length && s.Chars(c) = tst.Chars(i)  do
            c <- c + 1
            i <- i + 1
          i = tst.Length
        else false
      s |> find substring_comp (fun s -> s.Length - 1) dfa 

    static member find_subsequence (dfa : DDFA<int,_,string>[]) (s : byte []) = 
      let subseq_comp (s : byte[]) (at : int) (tst : string) = 
        if at + tst.Length <= s.Length then
          let mutable c = at
          let mutable i = 0
          while i < tst.Length && (s.[c] |> char) = tst.Chars(i) do
            c <- c + 1
            i <- i + 1
          i = tst.Length
        else false
      s |> find subseq_comp (fun s -> s.Length - 1) dfa


    static member k_paths (dfa : DDFA<int,_,char>[], idx : int, kstride : int) =
      let halting (x : DDFA<int,_,char>) parentdefer =
        if Option.isSome x.Label then 
          match x.Defer with
          | DefaultTransition _ -> 2  // This should stop all striding
          | Deferment _  -> 2 // This prevents skip accept states (once we handle skipped states we can change to 1)
        else
          match x.Defer with
          | DefaultTransition _ -> 2
          | Deferment x when x <> parentdefer -> 1
          | _ -> 0
      seq{
        if kstride > 0 then
          let current = dfa.[idx]
          let deferment = match current.Defer with DefaultTransition x -> x | Deferment x -> x
          if kstride = 1 then
            for (c,ptr) in current.Transitions do
              yield (string c, ptr)
          else
            for (c,ptr) in current.Transitions do
              let x = dfa.[ptr]
              let state = halting x  deferment 
              let c = string c
              if state = 2 then
                yield (c, ptr)
              elif state = 1 then
                yield (c, ptr)
                yield! MultiStride.k_paths (dfa,ptr,kstride-1) |> Seq.map (fun (a,b) -> (c + a,b))
              else
                yield! MultiStride.k_paths (dfa,ptr,kstride-1) |> Seq.map (fun (a,b) -> (c + a,b))
      } //|> Seq.map (fun (i,j) -> (i,List.rev j))

    static member compute_reachable (dfa : DDFA<int,_,string>[], idx : int) =
      let reached = new System.Collections.Generic.HashSet<int>(HashIdentity.Structural)
      let queue = new System.Collections.Generic.Queue<int>()
      let check i = 
        let s = reached.Add(i) 
        if s then queue.Enqueue(i)
        //else (eprintf "skipping %i" i)
      
      do check idx 
      //eprintfn "queue size : %i" queue.Count

      while queue.Count > 0 do
        let i = queue.Dequeue()
        //eprintfn "dequeuing %i" i
        let c = dfa.[i]
        match c.Defer with DefaultTransition x -> check x | Deferment x -> check x
        for (_,ptr) in c.Transitions do check ptr

      reached |> Seq.sort |> Array.ofSeq

    // this doesn't necessarily preserve hitting all the labels anymore (see k_paths).  This this needs better
    // treatment later.  Probably by propogating labels throughout the single stride DDFA before multistriding.
    static member expand_k_stride (dfa : DDFA<int,_,char>[], idx : int, kstride : int) =
      let c = dfa.[idx]
      let t = MultiStride.k_paths (dfa,idx,kstride)
              //|> Seq.map (fun (i,j) -> (i,List.head j)) // currently not dealing with label propogation
              |> Seq.sortByDescending (fun (i,_) -> i.Length)
      match c.Defer with
      | DefaultTransition x -> DDFA.DDFA<_,_,_>.root(c.Label,x,t)
      | Deferment x -> DDFA.DDFA<_,_,_>.defered(c.Label,x,t)

    static member k_stride (dfa : DDFA<int,_,char>[], kstride : int) =
      let make i = MultiStride.expand_k_stride(dfa,i,kstride)
      Array.init dfa.Length make

    // eliminates unreachable states (assumes that state 0 is start)
    static member k_stride_reduced (dfa : DDFA<int,_,char>[], kstride : int) =
      let make i = MultiStride.expand_k_stride(dfa,i,kstride)
      let original = Array.init dfa.Length make
      let reachable = MultiStride.compute_reachable (original,0)

      let lookup = 
        reachable
        |> Seq.mapi (fun newptr oldptr -> System.Collections.Generic.KeyValuePair<_,_>(oldptr,newptr))
        |> (fun i -> System.Collections.Generic.Dictionary<_,_>(i, HashIdentity.Structural))

      //for i in lookup do
      //  eprintfn "%i => %i" i.Key i.Value

      Array.init 
        reachable.Length
        (
          fun z ->
            let idx = reachable.[z]
            //eprintfn "Rewriting %i" idx
            let c = original.[idx]
            let t = c.Transitions |> Seq.map (fun (i,j) -> (i, lookup.[j]))
            match c.Defer with
            | DefaultTransition x ->
              DDFA.DDFA<_,_,_>.root(c.Label,lookup.[x],t)
            | Deferment x ->
              DDFA.DDFA<_,_,_>.defered(c.Label,lookup.[x],t)
        )

    end

