open! IStd
module F = Format
module L = Logging
open PulseDomainInterface

type label = 
 | Ok
 (* Some "sure" NPE will be this case, in single-function analysis *)
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

val from_lists_of_summary_label_pairs : 
    (AbductiveDomain.summary ExecutionDomain.base_t * label) list
    ->  t
