
(** [sbatch ?options script] creates a "sbatch" command that submits the script located in the path [script] with the command line options [?options]. *)
let sbatch ?(options = []) ?(wrap = false) target =
  Format.sprintf "%s %s"
    (Misc.mk_shell_cmd ~options "sbatch")
    (if wrap then Format.sprintf "--wrap=\"%s\"" target else target)

(** [srun ?options cmd] creates an "srun" command that executes the command [cmd] with the command line options [?options]. *)
let srun ?(options = []) cmd =
  Format.sprintf "%s %s"
    (Misc.mk_shell_cmd ~options "srun") cmd

(** [grep_job_id sbatch_cmd] Given a "sbatch" command, generates a command
    that extracts from the output of the "sbatch" command the ID of the job
    that was submitted. *)
let grep_job_id sbatch_cmd =
  Format.sprintf
    "%s | grep -oP \"^Submitted batch job \\K[0-9]+$\""
    sbatch_cmd

(** [scancel job_id] creates the command that runs "scancel" on [job_id]. *)
let scancel job_id =
  Format.sprintf "scancel %d" job_id
