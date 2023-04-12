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
 | ErrorRetainCycle
 | ErrorMemoryLeak of int
 | ErrorResourceLeak
 | ErrorInvalidAccess of int
 | ErrorException
 | ErrorOthers
 [@@deriving yojson_of]

type summary_post = (label * (AbductiveDomain.summary) option) [@@deriving yojson_of]

type t = summary_post list [@@deriving yojson_of]

val from_lists_of_summaries : 
    (AbductiveDomain.summary ExecutionDomain.base_t * label) option list
    ->  t
