
module Fmt = CCFormat

(** Description of a given task *)
type t = {
  name: string; (* name of this task *)
  synopsis: string option;
  action: Action.t;
}

let pp out (self:t) =
  let open Misc.Pp in
  let {name;synopsis;action} = self in
  Fmt.fprintf out "(@[<v1>task%a%a%a@])"
    (pp_f "name" pp_str) name
    (pp_opt "synopsis" pp_str) synopsis
    (pp_f "action" @@ Action.pp) action
