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
  open System

  type FDD<'p,'d> = 
    | Root of ('p * FDD<'p,'d>)[]
    | Decision of 'd
    with
      member self.NodeCount 
        with get() =
          let memory = System.Runtime.Serialization.ObjectIDGenerator()
          let rec test x = 
            let _id,first = memory.GetId(x)
            if first then 
              match x with 
              | Decision _ -> 1
              | Root a -> 1 + (a |> Seq.map snd |> Seq.sumBy test)
            else 0
          let reported = test self
          reported
      
      member self.Representation
        with get(ptoString, dtoString) =
          let memory = System.Runtime.Serialization.ObjectIDGenerator()
          let nodes  = System.Collections.Generic.Dictionary<_,_>()
          let rec test x = 
            let id',first = memory.GetId(x)
            let id = int id'
            if first then 
              let v = 
                match x with 
                | Decision a -> Terminal (id, dtoString a)
                | Root a -> Node (id, a |> Array.map (fun (c,d) -> (ptoString c, test d)) )
              do nodes.[id] <- v
              id
            else id
          let _ = test self
          nodes |> Seq.sortBy (fun i -> i.Key) |> Seq.map (fun i -> i.Value) |> Array.ofSeq

      member self.ToString(ptoString, dtoString) = 
        self.Representation(ptoString,dtoString)
        |> Seq.map (fun i -> sprintf "%s" <| string i) |> String.concat "\n"

      override self.ToString() = self.ToString(string,string)
         

  and FDD_Representation =
    | Node of int * ((string * int)[])
    | Terminal of int * string
    with
      override self.ToString() = 
        match self with
        | Node (a,b) -> b |> Seq.map (fun (c,d) -> sprintf "%s -> %i " c d) |> String.concat "; " |> sprintf "{node : %i; decisions : [%s]" a 
        | Terminal (a,b) -> sprintf "{node : %i; decision %s}" a b

  module Byte_FDD = begin

    open FDD.Inlines

    let rec getDecision (fdd : _, data : System.ReadOnlySpan<byte>) = 
        match fdd with
        | Root a when a.Length = 256 -> getDecision (a.[int data.[0]] |> snd, data.Slice(1))
        | Decision d -> d
        | _ -> failwith "FDD internal structure mismatch."

    let inline projectionArray newset (x : ^R seq) = 
      let rules = Array.ofSeq x
      if rules.Length = 0 then failwith "Empty rule list"
      let dimension = Predicate_Rules.len rules.[0]
      if rules |> Array.forall (fun r -> dimension = Predicate_Rules.len r) |> not then failwith "Rule predicate lengths are inconsistant"

      // build projection per dimension
      let project dim = 
        Array.init 
          256 
          (fun v ->
            let vb = byte v
            let bs = newset rules.Length
            rules 
            |> Array.iteri 
              (fun idx rule -> 
                if Predicate_Rules.test vb dim rule then 
                  Mutable_Sets.add idx bs
              )
            bs
          )

      [|
        for dim = 0 to dimension - 1 do
          project dim
      |]

    let inline ofRules toDecision newset (x : ^R seq) =
      let rules = Array.ofSeq x 
      let projections = projectionArray newset x
      let memories = Array.init projections.Length (fun _ -> System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural))
      let decisions = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
      // this isn't the fastest way to do this.
      let rec nextLevel mask lvl =
        if lvl < projections.Length then
          let memory = memories.[lvl]
        
          Array.init
            256
            (fun idx ->
              let result = projections.[lvl].[idx] |> Immutable_Sets.intersect mask
              let s, fdd = memory.TryGetValue(result)
              if s then
                (idx,fdd)
              else
                let fdd = nextLevel result (lvl+1)
                memory.[result] <- fdd
                (idx,fdd)
            ) |> Root

        else
          let s, fdd = decisions.TryGetValue(mask)
          if s then fdd
          else
            let fdd = toDecision mask |> Decision
            decisions.[mask] <- fdd
            fdd

      let fullmask = newset rules.Length
      for idx = 0 to rules.Length - 1 do Mutable_Sets.add idx fullmask

      nextLevel fullmask 0

  end
