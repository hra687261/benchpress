
type t = {
  username: string option;
  host: string;
}

let make ?username host =
  { username; host; }

let to_string { host; username; } =
  CCOption.map_or ~default:host (fun name -> name^"@"^host) username

let pp fmt self =
  Format.fprintf fmt "%s" (to_string self)

let wrap_cmd self cmd =
  let buf = Buffer.create 32 in
  Buffer.add_string buf "ssh ";
  Buffer.add_string buf (to_string self);
  Buffer.add_string buf " \""; (* echo remote_info_wrap_cmd >> here;  *)
  Buffer.add_string buf cmd;
  Buffer.add_string buf "\"";
  Buffer.contents buf

let paths_error ?loc () =
  Error.fail ?loc ("file and directory paths have to be absolute or use the default $HOME variable when running in remote mode")

let file_exists self path =
  if Sys.command (wrap_cmd self ("test -f "^path)) = 0
  then true
  else false

let directory_exists self path =
  if Sys.command (wrap_cmd self ("test -d "^path)) = 0
  then true
  else false

let get_sub_files self path ext =
  let cmd =
    "find "^path^" -type f -regex '"^ext^"'"
  in
  let ncmd = wrap_cmd self cmd in
  let rec read_lines acc inc =
    try
      let l = input_line inc in
      read_lines (l :: acc) inc
    with End_of_file -> acc
  in
  let inc = Unix.open_process_in ncmd in
  let files = read_lines [] inc in
  let _ = Unix.close_process_in inc in
  files
