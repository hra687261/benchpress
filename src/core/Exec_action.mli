
type cb_progress = <
  on_progress: percent:int -> elapsed_time:float -> eta:float -> unit;
  on_done: unit;
>

module Exec_run_provers : sig

  type expanded = {
    j: int;
    problems: Problem.t list;
    provers: Prover.t list;
    checkers: Proof_checker.t Misc.Str_map.t;
    limits : Limit.All.t;
    proof_dir: string option;
    db_file: string option;
  }

  val expand :
    ?j:int ->
    ?dyn:bool ->
    ?limits:Limit.All.t ->
    ?proof_dir:string ->
    ?interrupted:(unit -> bool) ->
    Definitions.t ->
    ?pb_file:string ->
    ?db_file:string ->
    Limit.All.t ->
    int option ->
    string option ->
    Subdir.t list ->
    Prover.t list ->
    expanded

  val run :
    ?timestamp:float ->
    ?on_start:(expanded -> unit) ->
    ?on_solve:(Test.result -> unit) ->
    ?on_start_proof_check:(unit -> unit) ->
    ?on_proof_check:(Test.proof_check_result -> unit) ->
    ?on_done:(Test_compact_result.t -> unit) ->
    ?interrupted:(unit -> bool) ->
    uuid:Uuidm.t ->
    save:bool ->
    expanded ->
    Test_top_result.t lazy_t * Test_compact_result.t
    (** Run the given prover(s) on the given problem set, obtaining results
        after all the problems have been dealt with.
        @param on_solve called whenever a single problem is solved
        @param on_done called when the whole process is done
    *)

  val run_sbatch_job :
    ?timestamp:float ->
    ?on_start:(expanded -> unit) ->
    ?on_solve:(Test.result -> unit) ->
    ?on_start_proof_check:(unit -> unit) ->
    ?on_proof_check:(Test.proof_check_result -> unit) ->
    ?on_done:(Test_compact_result.t -> unit) ->
    ?interrupted:(unit -> bool) ->
    ?config_file:string ->
    ?nodes:int ->
    ?ntasks:int ->
    ?cpus_per_task:int ->
    ?db_file:string ->
    uuid:Uuidm.t ->
    save:bool ->
    expanded ->
    Test_top_result.t lazy_t * Test_compact_result.t

end

module Progress_run_provers : sig
  type t = <
    on_res: Run_prover_problem.job_res -> unit;
    on_start_proof_check : unit;
    on_proof_check_res: Test.proof_check_result -> unit;
    on_done: unit;
  >
  val nil : t
  val make :
    ?cb_progress:cb_progress ->
    ?pp_results:bool ->
    ?dyn:bool ->
    Exec_run_provers.expanded -> t
  (** Make a progress tracker.
      @param dyn if true, print a progress bar in the terminal
      @param pp_results if true, print each individual result as it's found
      @param on_progress callback when progress is made, with a percentage and ETA
  *)
end

val dump_results_sqlite : Test_top_result.t -> unit

val run :
  ?save:bool ->
  ?interrupted:(unit -> bool) ->
  ?cb_progress:cb_progress -> Definitions.t -> Action.t -> unit
