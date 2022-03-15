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

  module Inlines = begin
    module Immutable_Sets = begin
      let inline add (item : ^I) (set : ^S when ^S : (member Add : ^I -> ^S)) = 
        (^S : (member Add : ^I -> ^S) (set,item) )
      let inline remove (item : ^I) (set : ^S when ^S : (member Remove : ^I -> ^S)) = 
        (^S : (member Remove : ^I -> ^S) (set,item) )
      let inline intersect (seta : ^S when ^S : (member Intersect : ^S -> ^S)) (setb : ^S) = 
        (^S : (member Intersect : ^S -> ^S) (seta,setb) )
      let inline union (seta : ^S when ^S : (member Union : ^S -> ^S)) (setb : ^S) = 
          (^S : (member Union : ^S -> ^S) (seta,setb) )
      let inline inverse (set : ^S when ^S : (member Inverse : unit -> ^S)) = 
        (^S : (member Inverse : unit -> ^S) (set) )
      let inline members (set : ^S when ^S : (member Members : unit -> ^I seq)) =
        (^S : (member Members : unit -> ^I seq)(set))
    end

    module Mutable_Sets = begin
      let inline add (item : ^I) (set : ^S when ^S : (member AddInPlace : ^I -> unit)) = 
        (^S : (member AddInPlace : ^I -> unit) (set,item) )
      let inline remove (item : ^I) (set : ^S when ^S : (member RemoveInPlace : ^I -> unit)) = 
        (^S : (member RemoveInPlace : ^I -> unit) (set,item) )
    end

    module Predicate_Rules = begin
      let inline len (x : ^R when ^R : (member Count : unit -> int)) = 
        (^R : (member Count : unit -> int)(x))
      let inline decision (x : ^R when ^R : (member Decision : unit -> ^D)) : ^D = 
        (^R :(member Decision : unit -> ^D)(x))
      let inline test (v : ^V) (idx : int) ( x : ^R when ^R : (member Test : int * ^V -> bool)) =
        (^R : (member Test : int * ^V -> bool)(x,idx,v))
      let inline intervals (idx : int) ( x : ^R when ^R : (member Intervals : int  -> (^V * ^V) seq)) =
        (^R : (member Test : int -> (^V * ^V) seq)(x,idx))
    end

  end