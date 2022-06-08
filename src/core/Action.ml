
open Common
module T = Test
type path = string

(** {1 Actions} *)

type run_provers = {
  db_file: string option;
  pb_file: string option;
  j: int option; (* number of concurrent processes *)
  dirs: Subdir.t list; (* list of directories to examine *)
  provers: Prover.t list;
  pattern: string option;
  limits : Limit.All.t;
  loc: Loc.t option;
}

type run_provers_slurm_submission = {
  nodes: int option;
  (* the number of nodes that need to be allocated for the job.
  *)
  ntasks: int option;
  (* number of parallel processes to launch *)
  cpus_per_task: int option;
  (* The number of CPUs in a node that will be allocated to the
     concurrent execution of benchpress.
     if [cpus_per_task] is provided and not [j], then the number of
     concurrent processes will be equal to the number of cpus per
     task.
  *)
  db_file: string option;
  (* the file in which to store the database *)
  j: int option;
  (* number of concurrent processes *)
  dirs: Subdir.t list; (* list of problems *)
  provers: Prover.t list;
  pattern: string option;
  limits : Limit.All.t;
  loc: Loc.t option;
}

type git_fetch_action =
  | Git_fetch
  | Git_pull

type git_checkout = {
  dir: string;
  ref: string;
  fetch_first: git_fetch_action option;
  loc: Loc.t;
}

(** An action to perform *)
type t =
  | Act_run_provers of run_provers
  | Act_run_slurm_submission of run_provers_slurm_submission
  | Act_git_checkout of git_checkout
  | Act_run_cmd of { cmd: string; loc: Loc.t; }
  | Act_progn of t list

let pp_run_provers out (self:run_provers) =
  let open Misc.Pp in
  let { db_file; pb_file; dirs; provers; limits; j; pattern; loc=_; }: run_provers = self in
  Fmt.fprintf out "(@[<v1>run_provers%a%a%a%a%a%a%a%a%a@])"
    (pp_opt "pb_file" Fmt.string) pb_file
    (pp_opt "db_file" Fmt.string) db_file
    (pp_f "dirs" (pp_l Subdir.pp)) dirs
    (pp_f "provers" (pp_l Prover.pp_name)) provers
    (pp_opt "pattern" pp_regex) pattern
    (pp_opt "timeout" Limit.Time.pp) limits.time
    (pp_opt "memory" Limit.Memory.pp) limits.memory
    (pp_opt "stack" Limit.Stack.pp) limits.stack
    (pp_opt "j" Fmt.int) j

let pp_run_provers_slurm out (self:run_provers_slurm_submission) =
  let open Misc.Pp in
  let {
    nodes; ntasks; cpus_per_task; db_file;
    j; dirs; provers; pattern; limits; _
  } = self
  in
  Fmt.fprintf out "(@[<v1>run_provers.Slurm%a%a%a%a%a%a%a%a%a%a%a@])"
    (pp_opt "nodes" Fmt.int) nodes
    (pp_opt "ntasks" Fmt.int) ntasks
    (pp_opt "cpus_per_task" Fmt.int) cpus_per_task
    (pp_opt "db_file" Fmt.string) db_file
    (pp_opt "j" Fmt.int) j
    (pp_f "dirs" (pp_l Subdir.pp)) dirs
    (pp_f "provers" (pp_l Prover.pp_name)) provers
    (pp_opt "pattern" pp_regex) pattern
    (pp_opt "timeout" Limit.Time.pp) limits.time
    (pp_opt "memory" Limit.Memory.pp) limits.memory
    (pp_opt "stack" Limit.Stack.pp) limits.stack

let pp_git_fetch out = function
  | Git_fetch -> Fmt.string out "fetch"
  | Git_pull -> Fmt.string out "pull"

let pp_git_checkout out (self:git_checkout) =
  let open Misc.Pp in
  let {dir;ref;fetch_first;loc=_} = self in
  Fmt.fprintf out "(@[<v1>git-checkout%a%a%a@])"
    (pp_f "dir" pp_regex) dir
    (pp_f "ref" pp_regex) ref
    (pp_opt "fetch-first" pp_git_fetch) fetch_first

let rec pp out (self:t) : unit =
  match self with
  | Act_run_provers a -> pp_run_provers out a
  | Act_run_slurm_submission a -> pp_run_provers_slurm out a
  | Act_git_checkout g -> pp_git_checkout out g
  | Act_run_cmd {cmd=s;loc=_} -> Fmt.fprintf out "(run-cmd %S)" s
  | Act_progn l -> Fmt.fprintf out "(@[%a@])" (Misc.Pp.pp_l pp) l

