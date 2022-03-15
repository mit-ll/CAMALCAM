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

  module AhoCorasick = begin
    let toDDFA (x : Aho_Corasick.AC_Table<'a>) =
      let strings = x.Index.Keys |> Seq.sort |> Array.ofSeq
      let lookup  = System.Collections.Generic.Dictionary<string,int>(strings |> Seq.mapi (fun j i -> System.Collections.Generic.KeyValuePair<_,_>(i,j)))
      let helper (i : char, j : Aho_Corasick.AC_Trie<_>) = (i,lookup.[j.Prefix])
      [|
        for p in strings do
          let trie = x.Index.[p]
          let label = 
            match trie.Label with  // we only grab the first label that could match at a position.
            | Some x -> Some x
            | None -> x.Index.[trie.DictSuffix].Label 
          if p = "" then
            yield DDFA.DDFA.root<_,_,_>(label,lookup.[p],trie.Children |> Seq.map helper)
          else
            yield DDFA.DDFA.defered<_,_,_>(label,lookup.[trie.Suffix],trie.Children |> Seq.map helper)
      |]
        
  end

