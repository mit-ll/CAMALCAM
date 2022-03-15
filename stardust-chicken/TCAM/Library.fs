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

  type TCAM_Byte(t,m) = 
    class
      let mutable target : byte = t &&& m
      let mutable mask : byte = m

      // **** **** constructor
      new() = TCAM_Byte(0uy,0uy)

      // Byte literal constructor
      new (bytevalue) = TCAM_Byte(bytevalue,0xffuy)

      // Shorthand to grab ascii from a char for a byte literal  Do not use with chars of value higher than 127
      new(literal : char) =
        let bytevalue = literal |> byte
        TCAM_Byte(bytevalue)

      // string encoding constructor
      new(expr : string) = 
        if expr.Length > 8 then failwith "string to large"
        let expr = expr + System.String('*',8 - expr.Length)
        let mutable t = 0uy
        let mutable m = 0uy
        for idx = 0 to 7 do
          t <- t <<< 1
          m <- m <<< 1
          match expr.Chars(idx) with
          | '0' -> t <- t ||| 0uy ; m <- m ||| 1uy
          | '1' -> t <- t ||| 1uy ; m <- m ||| 1uy
          | '*' -> () // every is zero
          | _ -> failwith "Invalid string format"

        TCAM_Byte(t,m)

      static member ofString (expr : string) = 
        seq{
          for idx in 0 .. 8 .. expr.Length - 1  do
            let s = expr.Substring(idx)
            if s.Length > 8 then TCAM_Byte(s.Substring(0,8))
            else TCAM_Byte(s)
        } |> Array.ofSeq

      member __.Literal with get() = TCAM_Byte(target,0xffuy)
      member __.LiteralByte with get() = target

      // tests a byte for a match against the tcam
      member __.Contains(test : byte) = target = (test &&& mask)
      
      override __.ToString() =
        let check i = if (((mask >>> i) &&& 1uy) = 0uy) then "*" else ((target >>> i) &&& 1uy).ToString() 
        seq{for i = 7 downto 0 do yield check i} |> String.concat "" 
    end

  and TCAM_Row<'d> = {
    bank     : TCAM_Byte[]
    decision : 'd
  }
    with 
      member self.Contains(test : byte []) =
        if self.bank.Length = test.Length then
          test |> Array.forall2 (fun (t1 : TCAM_Byte) t2 -> t1.Contains(t2)) self.bank
        else
          false

      member self.BankString() = self.bank |> Seq.map (fun i -> i.ToString()) |> String.concat " "
      // Duck interface
      member self.Count() = self.bank.Length
      member self.Decision() = self.decision
      member self.Test(index : int, value : byte) = self.bank.[index].Contains(value)
      member self.Test(index : int) =
        seq{
          let mutable a = 0uy
          let mutable b = 0uy
          let mutable first = true
          for i' = 0 to 255 do
            let i = byte i'
            if self.bank.[index].Contains(i) then
              if first then
                first <- false
                a     <- i
                b     <- i
              elif i=b+1uy then
                b <- i 
              else
                yield (a,b)
                a     <- i
                b     <- i
          // end for
          yield (a,b)
        }


  and TCAM_Module<'d>(rowsize:int) = class
      let rows = new System.Collections.Generic.List<TCAM_Row<'d>>()

      member __.Add(x : TCAM_Row<_>) = if x.bank.Length = rowsize then rows.Add(x) else failwith <| sprintf "bank size is incorrect %i<>%i" x.bank.Length rowsize
      member self.Add(x : seq<TCAM_Row<_>>) = for i in x do self.Add(i)

      member __.Rows with get() = rows

      member __.tryDecision with get(x : byte[]) =
        if x.Length = rowsize then
          rows |> Seq.tryPick (fun t -> if t.Contains(x) then Some t.decision else None)
        else
          failwith "bank size is incorrect"
    end

  module Utilities = begin
    module ShadowEncoder = begin
        open System.Collections.Generic
        
        // see 

        type Hufftree<'a> = 
          | Virtual of int64 * Hufftree<'a> * Hufftree<'a>
          | Leaf of int64 * 'a
          with
            member self.UniqueWeight with get() = match self with Virtual (x,_,_) -> x | Leaf (x,_) -> x
            member self.Weight with get() = (self.UniqueWeight >>> 32) |> int
            member self.Encoding with get() =
              seq{
                match self with
                | Leaf (_,a) -> yield ("",a)
                | Virtual (_,a,b) ->
                  yield! a.Encoding |> Seq.map (fun (i,j) -> ("0"+i,j))
                  yield! b.Encoding |> Seq.map (fun (i,j) -> ("1"+i,j))
              }
            static member Build((weightOf : _ -> int), x : seq<_>) =
              //let PQ = System.Collections.Generic.PriorityQueue<Hufftree<'a>,_>()
              let comp = 
                {
                  new IComparer<Hufftree<'b>> with 
                    member __.Compare(a' : Hufftree<'b>, b' : Hufftree<'b>) =
                      let l = a'.UniqueWeight 
                      let r = b'.UniqueWeight
                      let result = l.CompareTo(r)
                      //eprintfn "%i.compareTo(%i) = %i" l r result
                      result
                }
              let PQ = SortedSet<Hufftree<'a>>(comp)
              let unique =
                let mutable cnt = -1L
                fun (i : int) -> (cnt <- cnt + 1L); cnt + ((int64 i) <<< 32)
              let enqueue i = if PQ.Add(i) then () else failwith <| sprintf "value not unique %A" i
              let dequeue () = let x = PQ.Min in if PQ.Remove(x) then x else failwith "Set is not removing"
              x |> Seq.map (fun i -> Leaf(weightOf i |> unique,i)) |> Seq.iter enqueue

              while PQ.Count > 1 do
                let l = dequeue()
                let r = dequeue()
                let n = Virtual (unique (1 + System.Math.Max(l.Weight,r.Weight)),l,r)
                enqueue n

              dequeue()

        let encode_tree (x : Dictionary<int,List<int>>) (root : int) (number_states) =
          let L = Array.init number_states (fun _ -> "")
          let G = Array.init number_states (fun _ -> "")
          let W = Array.init number_states (fun _ -> 0)

          let rec push_globals code idx =
            let s,v = x.TryGetValue(idx)
            G.[idx] <- code + G.[idx]
            if s then 
              for c in v do
                push_globals code c

          let rec process_node idx =
            let s,v = x.TryGetValue(idx)
            if s then // we have work to do, otherwise this is the default and recursion ends
              // process children
              for c in v do
                process_node c
              // build huffman tree
              let HTree = Hufftree<_>.Build((fun i -> W.[i]), seq{yield idx; yield! v})
              let codes = Dictionary<_,_>(HTree.Encoding |> Seq.map (fun (code,index) -> KeyValuePair<_,_>(index,code)), HashIdentity.Structural)
              let actual_code weight local_length (candidate_code : string) =
                let bit_deficet = weight - (local_length + candidate_code.Length)
                candidate_code + System.String('0',bit_deficet)

              // Set weight of idx and L of idx
              W.[idx] <- HTree.Weight
              L.[idx] <- actual_code HTree.Weight 0 codes.[idx]
              // update globals of children (fix cases where globals don't have uniform length)
              for c in v do
                //let candidate_code = codes.[c]
                //let bit_deficet    = HTree.Weight - (L.[c].Length + candidate_code.Length)
                //let actual_code    = candidate_code + System.String('0',bit_deficet)
                push_globals (actual_code HTree.Weight L.[c].Length codes.[c]) c

          do process_node root

          seq{
            for i = 0 to number_states - 1 do
              let stateId = G.[i] + L.[i]
              let shadowCode = G.[i] + System.String('*',L.[i].Length)
              yield (i,stateId,shadowCode)
          }
          
      end

    type TCAMConvert = class

      // assumes start state is 0
      static member toTCAM (dfa : DDFA.DDFA<int,'a,char>[], stride : int) =
        if stride < 1 then failwith "stride must be positive"
        // simplify deferrments
        let _ = DDFA.Utilities.Optimzer.State_Expander.DeferChainReplacement dfa

        //make multi-stride DFA
        let dfa = DDFA.MultiStride.k_stride(dfa,stride)
        let tree, root = DDFA.Utilities.Deferment.get_forest dfa
        
        let codes = 
          if root.Count = 1 then
            ShadowEncoder.encode_tree tree (root.[0]) dfa.Length
          else
            tree.Add(dfa.Length,root) // forests get a virtual state to defer to
            ShadowEncoder.encode_tree tree (dfa.Length) (dfa.Length + 1)
        let codes = Array.ofSeq codes

        let lookup' = 
          codes 
          |> Seq.map (fun (i,j,k) -> System.Collections.Generic.KeyValuePair<_,_>(i,(TCAM_Byte.ofString(j) |> Array.map (fun i -> i.Literal),TCAM_Byte.ofString(k))))
        let lookup  = System.Collections.Generic.Dictionary<_,_>(lookup', HashIdentity.Structural)

        let banksize = (fst lookup.[0]).Length + stride

        let rows = 
          seq{
            for idx = 0 to dfa.Length - 1 do 
              let c = dfa.[idx]
              let stateCode, shadowMask = lookup.[idx]
              let rules = // Note that we aren't doing any range optimization in this yet.
                c.Transitions
                |> Seq.map 
                    (fun (pred, ptr) -> 
                      let tcampattern = 
                        Array.append
                          (pred |> Seq.map TCAM_Byte |> Array.ofSeq)
                          [| for _ = 1 to (stride - pred.Length) do yield TCAM_Byte() |]
                      let lbl = dfa.[ptr].Label
                      let gotocode, _ = lookup.[ptr]
                      {bank=tcampattern ; decision = (lbl,pred.Length,gotocode)}
                    )
                |> Array.ofSeq
              match c.Defer with
              | DDFA.Deferment _ -> 
                for r in rules do
                  let b = Array.append shadowMask r.bank
                  if b.Length <> banksize then failwith "Code incorrect"
                  yield {r with bank=b}
              | DDFA.DefaultTransition ptr when ptr = idx ->
                // generate actual transitions
                for r in rules do
                  let b = Array.append shadowMask r.bank
                  if b.Length <> banksize then failwith "Code incorrect"
                  yield {r with bank=b}
                // generate lookahead skip 
                for skip = 1 to stride - 1 do
                  for r in rules do
                    let skipper = r.bank.[0..r.bank.Length -  1 - skip] |> Array.append [|for _ = 1 to skip do yield TCAM_Byte()|] |> Array.append shadowMask
                    if skipper.Length <> banksize then failwith <| sprintf "Code incorrect %A <= %A" skipper r.bank
                    let decide =
                      let lbl,dskip,gotostate = r.decision 
                      if skip + dskip <= stride then
                        (lbl,skip+dskip,gotostate)
                      else
                        (c.Label, skip, stateCode)
                    yield {bank=skipper; decision = decide}
                let b = [|for _ = 1 to stride do yield TCAM_Byte()|] |> Array.append shadowMask
                if b.Length <> banksize then failwith "Code incorrect"
                yield {bank=b; decision = (c.Label, stride, stateCode)}
              | DDFA.DefaultTransition ptr -> 
                // generate actual transitions
                for r in rules do
                  let b = Array.append shadowMask r.bank
                  if b.Length <> banksize then failwith "Code incorrect"
                  yield {r with bank=b}
                // generate default transition 
                let lbl = dfa.[ptr].Label
                let gotocode, _ = lookup.[ptr]
                let b = [|for _ = 1 to stride do yield TCAM_Byte()|] |> Array.append shadowMask
                if b.Length <> banksize then failwith "Code incorrect"
                yield {bank=b; decision = (lbl, 1, gotocode)}
          } |> Seq.sortByDescending (fun i -> i.BankString()) |> Seq.distinctBy (fun i -> i.BankString()) |> Array.ofSeq

        let TCAM_mod = TCAM_Module(banksize)
        for r in rows do
          //eprintfn "Adding %A" r
          TCAM_mod.Add(r)
        (lookup.[0], TCAM_mod)

      end

  end