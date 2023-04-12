open! IStd
module F = Format
module L = Logging
type t = int list [@@deriving compare, equal, yojson_of]

val initialize : Location.t -> t

val add_next_loc : t -> Location.t -> t

(* Get the last location (line num) on a trace. *)
val get_last_loc: t -> int option

val pp : F.formatter -> t -> unit
