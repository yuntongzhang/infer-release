open! IStd
module F = Format
module L = Logging

type t = int list [@@deriving compare, equal, yojson_of]


let initialize ({line}: Location.t) = [ line ]


let add_next_loc trace (loc: Location.t) =
  if Location.is_dummy loc then trace
  else
    let new_line = loc.line in
    let rev_lines = List.rev trace in
    match rev_lines with
    | [] -> [ new_line ]
    | last_line :: _ ->
        if Int.equal last_line new_line then
          trace
        else
          List.rev (new_line :: rev_lines)


let get_last_loc trace =
  match List.rev trace with
  | [] -> None
  | last_line :: _ -> Some last_line


let pp fmt (trace: t) =
  F.pp_print_list Int.pp fmt trace
