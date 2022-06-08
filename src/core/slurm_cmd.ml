
let sbatch ?(options = []) cmds =
  let buf = Buffer.create 32 in
  Buffer.add_string buf "#!/bin/bash\n\n";
  List.iter (
    fun (k, v_opt) ->
      match v_opt with
      | Some v ->
        Buffer.add_string buf ("#SBATCH "^k^"="^v^"\n")
      | None ->
        Buffer.add_string buf (k^" ")
  ) options;
  List.iter (
    fun (cmd, amp) ->
      Buffer.add_string buf (cmd ^ if amp then " &\n" else "\n")
  ) cmds;
  Buffer.contents buf

let srun ?(options = []) cmd =
  let buf = Buffer.create 32 in
  Buffer.add_string buf "srun ";
  List.iter (
    fun (k, v_opt) ->
      match v_opt with
      | Some v ->
        Buffer.add_string buf (k^"="^v^" ")
      | None ->
        Buffer.add_string buf (k^" ")
  ) options;
  Buffer.add_string buf cmd;
  Buffer.contents buf

let bash cmd =
  let buf = Buffer.create 32 in
  Buffer.add_string buf "bash -c \"";
  Buffer.add_string buf cmd;
  Buffer.add_string buf "\"";
  Buffer.contents buf
