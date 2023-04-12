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
  | ErrorRetainCycle
  (* For leak and NPE, we have real "error" trace. So take the initial point of that trace. *)
  | ErrorMemoryLeak of int
  | ErrorResourceLeak
  | ErrorInvalidAccess of int
  | ErrorException
  | ErrorOthers
[@@deriving yojson_of]

type summary_post = (label * (AbductiveDomain.summary) option) [@@deriving yojson_of]

type t = summary_post list [@@deriving yojson_of]

(* From the computed summary with label, construct a structure for dumping information. *)
let construct_summary_post (summary_label : (AbductiveDomain.summary ExecutionDomain.base_t * label) option) =
  match summary_label with
    | None -> (ErrorException, None) (* None summary means exception happened*)
    | Some (summary, label) -> 
      (* The meta data in summary is already captured by labels;
         strip those and standardize the format of summary. *)
      match summary with
      | ContinueProgram astate
      | ExitProgram astate
      | AbortProgram {astate; _}
      | LatentAbortProgram {astate; _ }
      | LatentInvalidAccess {astate; _; }
      | ISLLatentMemoryError astate ->
        label, Some astate


let from_lists_of_summaries summary_labels =
  let result_list = List.map summary_labels ~f:construct_summary_post in
  result_list
