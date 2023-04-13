open! IStd
module F = Format
module L = Logging
open PulseDomainInterface

type label =
  | Ok
  (* for these 5, summary was already there. Take the initial point in their error trace. *)
  | AbortProgram of int
  | ExitProgram of int
  | LatentAbortProgram of int
  | LatentInvalidAccess of int
  | ISLLatentMemoryError of int
  | ErrorInvalidAccess of int
  | ErrorOthers
[@@deriving yojson_of]

type summary_labelled = (label * (AbductiveDomain.summary) option) [@@deriving yojson_of]

type t = summary_labelled list [@@deriving yojson_of]

(* From the computed summary with label, construct a structure for dumping information. *)
let construct_summary_post (summary_label : AbductiveDomain.summary ExecutionDomain.base_t * label) =
  let summary, label = summary_label in
  match summary with
    | ContinueProgram astate
    | ExitProgram astate
    | AbortProgram {astate; _}
    | LatentAbortProgram {astate; _ }
    | LatentInvalidAccess {astate; _; }
    | ISLLatentMemoryError astate ->
      label, Some astate


let from_lists_of_summary_label_pairs summary_labels =
  let result_list = List.map summary_labels ~f:construct_summary_post in
  result_list
