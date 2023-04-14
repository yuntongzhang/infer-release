open! IStd
module F = Format
module L = Logging
open PulseDomainInterface

type start_end_loc = int * int [@@deriving yojson_of]

type label =
  (* For these three, the two ints do not mean anything *)
  | Ok of start_end_loc
  | ExitProgram of start_end_loc
  | ISLLatentMemoryError of start_end_loc
  (* for these four, int * int are start and end of the error trace, respectively. *)
  | AbortProgram of start_end_loc
  | LatentAbortProgram of start_end_loc
  | InvalidAccess of start_end_loc
  | LatentInvalidAccess of start_end_loc
[@@deriving yojson_of]

type summary_labelled = label * AbductiveDomain.summary option [@@deriving yojson_of]

type t = summary_labelled list [@@deriving yojson_of]

(* From the computed summary with label, construct a structure for dumping information. *)
let construct_summary_post (summary_label : AbductiveDomain.summary ExecutionDomain.base_t * label)
    =
  let summary, label = summary_label in
  match summary with
  | ContinueProgram astate
  | ExitProgram astate
  | AbortProgram {astate; _}
  | LatentAbortProgram {astate; _}
  | LatentInvalidAccess {astate; _}
  | ISLLatentMemoryError astate ->
      (label, Some astate)


let from_lists_of_summary_label_pairs summary_labels =
  let result_list = List.map summary_labels ~f:construct_summary_post in
  result_list
