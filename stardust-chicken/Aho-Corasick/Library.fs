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
  // Implementation reference for data structure https://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_algorithm

  type AC_Table<'a> = {
    Index : System.Collections.Generic.Dictionary<string,AC_Trie<'a>>
    Trie  : AC_Trie<'a> 
  }
    with
      static member Create<'a>(dictionary : seq<string*'a>) =

        let inline init_trie prefix = {Prefix=prefix; Children = new System.Collections.Generic.List<_>(); Label=None; Suffix=""; DictSuffix=""} 
        let root : AC_Trie<'a> = init_trie ""
        let idx  = new System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        do  idx.Add("",root)

        let rec insert_string current action =
          let s, v = idx.TryGetValue(current)
          if s then
            match action with
            | Choice1Of2 l -> 
              v.Label <- 
                match v.Label with 
                | None -> Some l 
                | _ -> failwith <| sprintf "Label for string %s already set" current
            | Choice2Of2 (next,node) -> 
              v.Children.Add(next,node)
          else
            let v = init_trie current
            match action with
            | Choice1Of2 l -> v.Label <- Some l
            | Choice2Of2 (next,node) -> v.Children.Add(next,node)

            let parent = current.Substring(0,current.Length - 1)
            let next   = current.Chars(current.Length - 1)

            idx.Add(current,v)
            insert_string parent (Choice2Of2 (next,v))
        // End insert_string

        for s,l in dictionary do insert_string s (Choice1Of2 l)

        // Compute longest suffixes
        for i in idx do
          let v = i.Value
          let prefix = v.Prefix
          v.Suffix <- 
            seq{
              for i = 1 to prefix.Length - 1 do yield prefix.Substring(i)
              yield ""
            } |> Seq.find (fun p -> idx.ContainsKey(p))

        // Compute dict suffixes
        let rec find_dict i = 
          if i = "" then ""
          else
            let v = idx.[i]
            match v.Label with
            | Some _ -> v.Prefix
            | None -> find_dict v.Suffix
        for i in idx do
          let v = i.Value in v.DictSuffix <- find_dict v.Suffix

        // Return complete structure
        {Index=idx; Trie=root}

      static member Create(dictionary : seq<string>) =
        dictionary |> Seq.map (fun i -> (i,())) |> AC_Table<_>.Create


  and AC_Trie<'a> = {
    Prefix : string
    Children : System.Collections.Generic.List<char * AC_Trie<'a>>
    mutable Label : Option<'a> 
    mutable Suffix : string
    mutable DictSuffix : string
  }

  module Utilities = begin
    let inline private find finder (ac : AC_Table<_>) (s : string) =
      seq{
        let mutable current = ac.Trie
        for i = 0 to s.Length - 1 do
          let mutable repeat = true
          let test = s.Chars(i)
          let inline predicate (c,next) = if c=test then Some next else None
          while repeat do
            match current.Children |> Seq.tryPick predicate with
            | Some nxt -> 
              current <- nxt
              repeat  <- false
            | None when current.Prefix.Length > 0 ->
              current <- ac.Index.[current.Suffix]
            | _ -> repeat <- false
          // end do check for matches
          yield! finder ac i current 
      }

    let inline private find_byte finder (ac : AC_Table<_>) (s : byte []) =
      seq{
        let mutable current = ac.Trie
        for i = 0 to s.Length - 1 do
          let mutable repeat = true
          let test = s.[i] |> char
          let inline predicate (c,next) = if c=test then Some next else None
          while repeat do
            match current.Children |> Seq.tryPick predicate with
            | Some nxt -> 
              current <- nxt
              repeat  <- false
            | None when current.Prefix.Length > 0 ->
              current <- ac.Index.[current.Suffix]
            | _ -> repeat <- false
          // end do check for matches
          yield! finder ac i current 
      }

    let inline private finder (ac : AC_Table<_>) (i : int) (current : AC_Trie<_>) =
      seq{
        match current.Label with
        | Some _ -> yield (current.Prefix, i)
        | _ -> ()
        let mutable c = current
        while c.DictSuffix <> "" do
          yield (c.DictSuffix, i)
          c <- ac.Index.[c.DictSuffix]
      }


    let find_substrings (ac : AC_Table<_>) s = find finder ac s
    let find_subsequence (ac : AC_Table<_>) s = find_byte finder ac s 

  end