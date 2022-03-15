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

namespace FDD.Sets

  type BitSet(source : System.Collections.BitArray) =
    static member Empty (size:int) = System.Collections.BitArray(size) |> BitSet
    member __.get_array () =
      let ints = System.Math.Ceiling((float source.Length) / 32.) |> int
      let a : int32[] = Array.zeroCreate ints
      source.CopyTo(a,0)
      a
    member s.isEqual (i : BitSet) =
      let a = s.get_array()
      let b = i.get_array()
      a=b
    member __.Parent with get() = source
    member __.AddInPlace(v : int) : unit = source.[v] <- true
    member __.RemoveInPlace(v : int) : unit = source.[v] <- false
    member __.Add(v : int) : BitSet = 
      let bs = System.Collections.BitArray(source) |> BitSet
      bs.Parent.[v] <- false
      bs
    member __.Remove(v : int) : BitSet = 
      let bs = System.Collections.BitArray(source) |> BitSet
      bs.Parent.[v] <- false
      bs
    member __.Intersect(v : BitSet) = let r = System.Collections.BitArray(source) in (r.And(v.Parent) |> BitSet)
    member __.Union(v : BitSet) = let r = System.Collections.BitArray(source) in (r.Or(v.Parent) |> BitSet)
    member __.Inverse() = System.Collections.BitArray(source).Not() |> BitSet
    member __.Members() = 
      seq{
        let mutable idx = 0
        for i in source do 
          if i then yield idx
          idx <- idx + 1
      }
    override s.GetHashCode() = 
      s.get_array ()
      |> Seq.fold (fun (s:int) (i:int) -> ((s*7) + i) &&& System.Int32.MaxValue) 0
    override s.Equals(o:obj) =
      match o with 
      :? BitSet as t -> s.isEqual(t)
      | _ -> false
    interface System.IEquatable<BitSet> with
      member s.Equals(b:BitSet) = s.isEqual(b) 



