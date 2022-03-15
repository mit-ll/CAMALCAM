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

module Common
  
  open System.IO
  open System.Security.Cryptography
  open System.Text 

  let (|Prefix|_|) (p:string) (s:string) =
      if s.StartsWith(p) then
          Some(s.Substring(p.Length))
      else
          None
  let print_or_write (fd: Option<StreamWriter>) (data: string) =
    match fd with
      | Some(a) -> a.WriteLine(data)
      | None -> printfn "%s" data

  let maybe_write (fd: Option<StreamWriter>) (data: string) =
    match fd with
      | Some(a) -> a.WriteLine(data)
      | None -> ()

  let maybe_print(do_print: bool) (data: string) =
    match do_print with
    | true -> printfn "%s" data
    | false -> ()

  // Compare a result and reference file
  // The comparison steps line-by-line through result_fn (skipping lines beginning with >=2 '#' symbols).
  // In tandem, it steps through the reference and checks for the corresponding line. 
  // The comparison fails iff:
  // 1. The next item in ref_fn does match that of result_fn
  // 2. They contain different amounts of data
  // TODO This isn't the most efficient, so this should probably do some type of async compare where
  // you grab one line at a time from each when you need it (like in Python calling next on a generator)
  let compare_result_files(result_fn: string, ref_fn: string) =
    try
      let result = System.IO.File.ReadLines(result_fn) |> Seq.filter(fun (s) -> not(s.StartsWith("##") && not(s.Trim() = "")))
      let ref =  System.IO.File.ReadLines(ref_fn) |> Seq.filter(fun (s) -> not(s.StartsWith("##") && not(s.Trim() = "")))

      let mutable i = 0
      let mutable same = true
      let mutable complete = false

      let iresult = result.GetEnumerator()
      let iref = ref.GetEnumerator()
      let x = iresult.MoveNext()
      let y = iref.MoveNext()
      if (x && not(y)) || (not(x) && y) then
        same <- false
        complete <- true
      elif not x && not y then
        complete <- true

      while same && not complete do
        let a: string = iresult.Current.Trim()
        let b: string = iref.Current.Trim()
        if not(a.Equals(b)) then
          eprintfn "got: %s, should be: %s" a b
          same <- false
        let res_has_more = iresult.MoveNext()
        let ref_has_more = iref.MoveNext()
        if (res_has_more && not(ref_has_more)) || (not(res_has_more) && ref_has_more) then
          same <- false
        if not res_has_more && not ref_has_more then
          complete <- true
        i <- i + 1
      (same,i)
    with
      | _ -> failwith(sprintf "Error comparing %s and %s" result_fn ref_fn)