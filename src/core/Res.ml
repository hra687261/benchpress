(* This file is free software. See file "license" for more details. *)

module Fmt = CCFormat

type res =
  | Sat
  | Unsat
  | Unknown
  | Timeout
  | Error
  | Tag of string

type t = {
  res: res;
  steps: int option;
}

let mk ?steps res = { res; steps; }

let to_string { res; _ } =
  match res with
  | Sat -> "sat"
  | Unsat -> "unsat"
  | Unknown -> "unknown"
  | Timeout -> "timeout"
  | Error -> "error"
  | Tag s -> s

let of_string ?steps ~tags s =
  { res =
      begin match s with
        | "sat" -> Sat
        | "unsat" -> Unsat
        | "error" -> Error
        | "timeout" -> Timeout
        | "unknown" -> Unknown
        | s when List.mem s tags -> Tag s
        | s -> failwith ("unknown result: " ^ s)
      end;
    steps;
  }

let pp out s = Fmt.string out (to_string s)

let compare a b = match a.res, b.res with
  | Unsat, Unsat
  | Sat, Sat
  | (Unknown | Timeout), (Unknown | Timeout)
  | Error, Error -> `Same
  | Tag s1, Tag s2 when s1=s2 -> `Same
    (*
  | Unknown, Timeout -> `LeftBetter
  | Timeout, Unknown -> `RightBetter
       *)
  | (Unknown | Timeout | Error), (Sat | Unsat | Tag _) -> `RightBetter
  | (Sat | Unsat | Tag _), (Unknown | Timeout | Error) -> `LeftBetter
  | Error, (Unknown | Timeout) -> `RightBetter
  | (Unknown | Timeout), Error -> `LeftBetter
  | Unsat, Tag _
  | Sat, Tag _
  | Tag _, Unsat
  | Tag _, Sat
  | Unsat , Sat
  | Sat, Unsat
  | Tag _, Tag _
    -> `Mismatch

