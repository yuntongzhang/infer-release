(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open PulseBasicInterface
open PulseDomainInterface
module SummaryLabelled = PulseSummaryLabelled

type t = ExecutionDomain.summary list [@@deriving yojson_of]

let pp fmt summary =
  F.open_vbox 0 ;
  F.fprintf fmt "%d pre/post(s)@;" (List.length summary) ;
  List.iteri summary ~f:(fun i (pre_post : ExecutionDomain.summary) ->
      F.fprintf fmt "#%d: @[%a@]@;" i ExecutionDomain.pp (pre_post :> ExecutionDomain.t) ) ;
  F.close_box ()


(* Since the error label requires some addtional information that is created here, 
  bundle those information in this function first. *)
let exec_summary_of_post_common tenv ~continue_program proc_desc err_log
    (exec_astate : ExecutionDomain.t) : (_ ExecutionDomain.base_t * SummaryLabelled.label) option =
  match exec_astate with
  | ContinueProgram astate -> (
    match AbductiveDomain.summary_of_post tenv proc_desc astate with
    | Unsat ->
        None
    | Sat (Ok astate) ->
        Some (continue_program astate, SummaryLabelled.Ok)
    | Sat (Error (`PotentialInvalidAccessSummary (astate, address, must_be_valid))) -> (
      match
        AbductiveDomain.find_post_cell_opt address (astate :> AbductiveDomain.t)
        |> Option.bind ~f:(fun (_, attrs) -> Attributes.get_invalid attrs)
      with
      | None ->
          let trace_start_line = (Trace.get_start_location must_be_valid).line in
          Some (LatentInvalidAccess {astate; address; must_be_valid; calling_context= []}, SummaryLabelled.LatentInvalidAccess trace_start_line)
      | Some (invalidation, invalidation_trace) ->
          let summary = PulseReport.report_summary_error tenv proc_desc err_log
            (ReportableError
               { diagnostic=
                   AccessToInvalidAddress
                     { calling_context= []
                     ; invalidation
                     ; invalidation_trace
                     ; access_trace= must_be_valid }
               ; astate })
          in
          let error_trace_start = (Trace.get_start_location invalidation_trace).line
          in Some (summary, SummaryLabelled.ErrorInvalidAccess error_trace_start) ) )
  (* already a summary but need to reconstruct the variants to make the type system happy :( *)
  | AbortProgram {astate; error_trace_start} ->
    let trace_start_line = AbductiveDomain.get_last_line_in_trace astate in
      Some (AbortProgram {astate; error_trace_start}, SummaryLabelled.AbortProgram trace_start_line)
  (* TODO: labels below currently use the last location in the program path, which is wrong 
     These types should be coupled with necessary field, to get the object lifetime start locations *)
  | ExitProgram astate ->
    let last_trace_line = AbductiveDomain.get_last_line_in_trace astate in
      Some (ExitProgram astate, SummaryLabelled.ExitProgram last_trace_line)
  | LatentAbortProgram {astate; latent_issue} ->
    let last_trace_line = AbductiveDomain.get_last_line_in_trace astate in
      Some (LatentAbortProgram {astate; latent_issue}, SummaryLabelled.LatentAbortProgram last_trace_line)
  | LatentInvalidAccess {astate; address; must_be_valid; calling_context} ->
    let last_trace_line = AbductiveDomain.get_last_line_in_trace astate in
      Some (LatentInvalidAccess {astate; address; must_be_valid; calling_context}, SummaryLabelled.LatentInvalidAccess last_trace_line)
  | ISLLatentMemoryError astate ->
    let last_trace_line = AbductiveDomain.get_last_line_in_trace astate in
      Some (ISLLatentMemoryError astate, SummaryLabelled.ISLLatentMemoryError last_trace_line)


let force_exit_program tenv proc_desc err_log post =
  let summaries_with_labels = exec_summary_of_post_common tenv proc_desc err_log post ~continue_program:(fun astate ->
      ExitProgram astate )
  in match summaries_with_labels with
    | None -> None
    | Some inner -> Some (fst inner)


let write_summary_labelled_json summaries_with_labels =
  let summary_labelled = SummaryLabelled.from_lists_of_summary_label_pairs summaries_with_labels in
  let json_summary_labelled = [%yojson_of: SummaryLabelled.t] summary_labelled in
  let f_json json_content fname = Yojson.Safe.to_file fname json_content;
  in
  f_json json_summary_labelled "summary_labelled.json"


let of_posts tenv proc_desc err_log posts =
  let summaries_with_labels = List.filter_mapi posts ~f:(fun i exec_state ->
      L.d_printfln "Creating spec out of state #%d:@\n%a" i ExecutionDomain.pp exec_state ;
      exec_summary_of_post_common tenv proc_desc err_log exec_state ~continue_program:(fun astate ->
          ContinueProgram astate ) )
  in
  if Config.pulse_fix_mode then write_summary_labelled_json summaries_with_labels ;
  List.map summaries_with_labels ~f:fst
