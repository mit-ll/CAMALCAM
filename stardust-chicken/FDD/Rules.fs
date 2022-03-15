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

namespace FDD.Rules

  type Interval<'inttype,'d when 'inttype : comparison> = {
    m_predicate : ('inttype * 'inttype) []
    m_decision  : 'd
    mutable permutation : int []
  } with
      member inline self.Item(idx : int) =
        if self.permutation.Length = self.m_predicate.Length then self.m_predicate.[self.permutation.[idx]]
        else self.m_predicate.[idx]
      member self.Count () = self.m_predicate.Length
      member self.Decision () = self.m_decision
      member self.Test (idx : int,value : 'inttype) = 
        let a,b = self.[idx]
        a <= value && value <= b
      member self.Test (idx : int) = self.[idx]
      static member Make(pred: ('inttype * 'inttype) [], decision : 'd) =
        {m_predicate=pred; m_decision=decision; permutation=[||]}
      static member Make(pred: ('inttype * 'inttype) [], decision : 'd, permutation : int[]) =
        {m_predicate=pred; m_decision=decision; permutation=permutation}
      override self.ToString() =
        sprintf "[[ %s -> %s ]]" 
          (
            seq{for i = 0 to self.Count() - 1 do self.[i]} 
            |> Seq.map (fun (i,j) -> sprintf "{%s-%s}" (string i) (string j) )
            |> String.concat ", "
          ) 
          (string self.m_decision)
  

