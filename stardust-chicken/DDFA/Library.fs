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

  type AndElse<'a> = Deferment of 'a | DefaultTransition of 'a
  and  DDFA<'indexptr,'label,'alpha> ={
    Label : Option<'label>
    Defer : AndElse<'indexptr>
    Transitions : System.Collections.Generic.List<'alpha * 'indexptr>
  }
    with
      member self.IsRoot with get () = match self.Defer with DefaultTransition _ -> true | _ -> false
      member self.IsDefer with get () = match self.Defer with Deferment _ -> true | _ -> false
      static member root<'i,'l,'a> (label : option<'l>, defaultptr : 'i, transitions : seq<'a*'i>) =
        {Label=label; Defer = DefaultTransition defaultptr; Transitions = System.Collections.Generic.List<_>(transitions)}
      static member defered<'i,'l,'a> (label : option<'l>, deferptr : 'i, transitions : seq<'a*'i>) =
        {Label=label; Defer = Deferment deferptr; Transitions = System.Collections.Generic.List<_>(transitions)}


  module Utilities = begin
    type Serializers() = class
        static let  quote x = sprintf "\"%s\"" x
        static let jlist x = "[" + (String.concat ", " x) + "]"
        static let jseq  (x : #seq<string>) = 
          seq{
            let it = x.GetEnumerator()
            yield "["
            let mutable next = it.MoveNext()
            while next do
              let c = it.Current
              next <- it.MoveNext()
              if next then yield (c + ",") else yield c
            yield "]"
          }
        static let serialize indexserialize labelserialize alphaserialize (x :DDFA<_,_,_>) =
          let labelname = quote "Label"
          let label = match x.Label with Some x -> labelserialize x | None -> "null"
          let defername, deferptr = match x.Defer with Deferment x -> (quote "Defer", indexserialize x) | DefaultTransition x -> (quote"AndElse", indexserialize x)
          let transitionname = quote "Transitions"
          let transitions = x.Transitions |> Seq.map (fun (i,j) -> [alphaserialize i; indexserialize j] |> jlist) |> jlist
          sprintf "{%s:%s, %s:%s, %s:%s}" labelname label defername deferptr transitionname transitions
          
        static let serialize_seq_int labelserialize alphaserialize (x :seq<DDFA<_,_,_>>) =
          let serial = serialize (fun x -> x.ToString()) labelserialize alphaserialize
          let gen i j = sprintf "{%s:%s,%s:%s}" (quote "Index") (i.ToString()) (quote "State") (serial j)
          x |> Seq.mapi gen |> jseq

        static member ToJson (x :seq<DDFA<int,unit,char>>) = serialize_seq_int (fun () -> "true") (string >> quote) x
        static member ToJson (x : seq<DDFA<int,unit,string>>) = serialize_seq_int (fun () -> "true") quote x
        static member ToJson (x :seq<DDFA<int,string,char>>) = serialize_seq_int quote (string >> quote) x
        static member ToJson (x :seq<DDFA<int,string,string>>) = serialize_seq_int quote quote x  

      end
    
    let inline private find streamat streamend (dfa : DDFA<int,_,'b>[]) (s : 'a) =
      seq{
        let mutable state = dfa.[0] 
        let e = streamend s
        for i = 0 to e do
          let mutable repeat = true
          let test : 'b = streamat s i
          let inline predicate (c,next) = if c=test then Some next else None
          while repeat do
            if state.Transitions.Count = 256 then
              let nxt = state.Transitions.[int test] |> snd
              if nxt >= 0 then 
                state <- dfa.[nxt]
                repeat  <- false
              else
                match state.Defer with
                | Deferment nxt -> 
                  state <- dfa.[nxt]
                  repeat <- true
                | DefaultTransition nxt ->
                  state <- dfa.[nxt]
                  repeat <- false
            else
              match state.Transitions |> Seq.tryPick predicate with
              | Some nxt -> 
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
          | Some x -> yield (x,i)
          | None -> ()
      }

    let find_substrings (dfa : DDFA<int,_,char>[]) (s : string) = 
      s |> find (fun s i -> s.Chars(i)) (fun s -> s.Length - 1) dfa 
    let find_subsequence (dfa : DDFA<int,_,char>[]) (s : byte []) = 
      s |> find (fun s i -> s.[i] |> char) (fun s -> s.Length - 1) dfa

    module Optimzer = begin
      open System.Collections.Generic
      type State_Expander() = class 
          static member Expand (x : DDFA<int,_,char>) =
            let keys = x.Transitions |> Seq.map (fun (i,j) -> KeyValuePair<int,int>(int i, j))
            let lookup = new Dictionary<int,int> (keys) 
            match x.Defer with
            | Deferment nxt ->
              let init = (fun i -> let s,v = lookup.TryGetValue(i) in if s then (char i, v) else (char i, -1) )
              DDFA.defered<_,_,_>(x.Label,nxt,Array.init 256 init)
            | DefaultTransition nxt ->
              let init = (fun i -> let s,v = lookup.TryGetValue(i) in if s then (char i, v) else (char i, nxt) )
              DDFA.root<_,_,_>(x.Label,nxt,Array.init 256 init)

          static member DeferChainReplacement (dfa : DDFA<int,_,char>[], idx : int ) =
            let current = dfa.[idx]
            match current.Defer with
            | DefaultTransition _ -> false // no Chaining
            | Deferment nxt ->
              let toSet (x : DDFA<int,_,char>) = x.Transitions |> Seq.map fst |> Set.ofSeq
              let matchpattern = toSet current
              let mutable j = nxt
              let mutable c = dfa.[j]
              while c.IsDefer && ((toSet c) = matchpattern) && (c.Transitions.Count < 256) do
                j <- match c.Defer with Deferment x -> x | _ -> failwith "algorithm fault"
                c <- dfa.[j]
              if j <> nxt then // we have to replace the dfa state.
                dfa.[idx] <- DDFA.defered<_,_,_>(current.Label,j,current.Transitions)
                true
              else
                false

          static member DeferChainReplacement (dfa : DDFA<int,_,char>[]) = 
            let mutable c = 0
            for i = 0 to dfa.Length - 1 do
              if State_Expander.DeferChainReplacement(dfa,i) then c <- c + 1
            c
        end

    end

    module Deferment = begin
      open System.Collections.Generic

      let get_forest (dfa : DDFA<int,_,_>[]) =
        let tree = new Dictionary<int,List<int>>(HashIdentity.Structural)
        let roots = new List<int>()
        for i = 0 to dfa.Length - 1 do
          match dfa.[i].Defer with
          | DefaultTransition _ -> roots.Add(i)
          | Deferment x ->
            let s,v = tree.TryGetValue(x)
            if s then v.Add(i)
            else 
              let v = new List<int>(i |> Seq.singleton)
              tree.[x] <- v
        (tree, roots)

    end

  end
