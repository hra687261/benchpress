(** {1 S-expressions with locations} *)

type pos = {line: int; col: int}

type loc = {
  file: string;
  start: pos;
  stop: pos;
}

let noloc = {file=""; start={line=1;col=1}; stop={line=1;col=1}}

let pp_loc out (loc:loc) : unit =
  if loc.start.line=loc.stop.line then (
    Format.fprintf out "%s:%d.%d-%d" loc.file loc.start.line loc.start.col loc.stop.col
  ) else (
    Format.fprintf out "%s:%d.%d-%d.%d"
      loc.file loc.start.line loc.start.col loc.stop.line loc.stop.col
  )

type t = {
  loc: loc;
  view: view;
}
and view =
  | Atom of string
  | List of t list

(** {2 Serialization and helpers} *)

include (CCSexp.Make(struct
    type nonrec loc=loc
    type nonrec t=t

    let make_loc =
      Some (fun (l1,c1)(l2,c2) file : loc ->
          {file; start={line=l1;col=c1};stop={line=l2;col=c2}})

    let atom_with_loc ~loc s : t= {loc; view=Atom s}
    let list_with_loc ~loc l : t = {loc; view=List l}
    let atom = atom_with_loc ~loc:noloc
    let list = list_with_loc ~loc:noloc

    let match_ s ~atom ~list =
      match s.view with
      | Atom s -> atom s
      | List l -> list l
  end) : CCSexp.S with type t := t)

(** {2 Decoder} *)

module D = Decoders.Decode.Make(struct
    type value = t
    let to_list = of_list
    let pp = pp
    let of_string = parse_string
    let of_file = parse_file
    let get_string s = match s.view with Atom s -> Some s | List _ -> None
    let get_ ~f s = match s.view with
      | Atom s -> (try Some (f s) with _ -> None)
      | List _ -> None
    let get_int s = get_ ~f:int_of_string s
    let get_float s = get_ ~f:float_of_string s
    let get_bool s = get_ ~f:bool_of_string s
    let get_null s = match s.view with Atom "null" | List [] -> Some () | _ -> None
    let get_list s = match s.view with List l -> Some l | Atom _ -> None

    let get_key_value_pairs s = match s.view with
      | List l ->
        (try
           Some
             (List.map
                (function {view=List [k; v];_} -> k,v | _ -> raise Exit)
                l)
         with Exit -> None)
      | Atom _ -> None
  end)

(** {2 Encoder} *)

module E = Decoders.Encode.Make(struct
    type value = t
    let to_string = to_string
    let of_string s = {loc=noloc; view=Atom s}
    let of_int = of_int
    let of_float = of_float
    let of_bool = of_bool
    let of_list = of_list
    let null = of_list []
    let of_key_value_pairs l = of_list (List.map (fun (k,v) -> of_list [k;v]) l)
  end)