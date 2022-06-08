
(** [sbatch_script ?options cmds] creates a sbatch script with [?options] as the sbatch options and cmds as the commands that the script runs. *)
let sbatch_script ?(options = []) cmds =
  let buf = Buffer.create 32 in
  Buffer.add_string buf "#!/bin/bash\n\n";
  List.iter (
    fun (k, v_opt) ->
      Buffer.add_string buf (
        "#SBATCH" ^
        begin match v_opt with
          | Some v -> " "^k^" "^v
          | None -> " "^k
        end ^ "\n"
      )
  ) options;
  Buffer.add_string buf "\n";
  List.iter (
    fun (cmd, amp) ->
      Buffer.add_string buf (cmd ^ if amp then " &\n" else "\n")
  ) cmds;
  Buffer.contents buf

(** [sbatch ?options script] creates a "sbatch" command that submits the script located in the path [script] with the command line options [?options]. *)
let sbatch ?(options = []) script =
  Format.sprintf "%s %s"
    (Misc.Shell.mk_cmd ~options "sbatch") script

(** [srun ?options cmd] creates an "srun" command that executes the command [cmd] with the command line options [?options]. *)
let srun ?(options = []) cmd =
  Format.sprintf "%s %s"
    (Misc.Shell.mk_cmd ~options "srun") cmd
