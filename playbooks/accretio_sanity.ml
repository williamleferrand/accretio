(*
 * accretio sanity
 *
 * this playbook wakes up periodically and run sanity checks
 *
 * william@accret.io
 *
 *)

open Lwt

open Printf
open CalendarLib

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers


let author = "william@accret.io"
let name = "Accretio sanity"
let description = "Accretio sanity checks"

let version = 0

(* the checks *****************************************************************)

let test_api_member_functions context () =
  context.log_info "testing api member functions" ;
  let unique_tag = "thistagisunique" in
  let member = 42 in
  lwt _ = context.remove_member ~member in
  match_lwt context.search_members ~query:unique_tag () with
  | [] ->
    begin
      lwt _ = context.add_member ~member in
      lwt _ = context.tag_member ~member ~tags:[ unique_tag ] in
      match_lwt context.search_members ~query:unique_tag () with
      | [ 42 ] ->
        begin
          lwt _ = context.add_member ~member in
          match_lwt context.search_members ~query:unique_tag () with
          | [ 42 ] ->
            begin
              match_lwt context.check_tag_member ~member ~tag:unique_tag with
              | false -> return `Failure
              | true ->
                lwt _ = context.remove_member ~member in
                match_lwt context.search_members ~query:unique_tag () with
                | [] -> return `Success
                | _ -> return `Failure
            end
          | _ -> return `Failure
        end
      | _ -> return `Failure
    end
  | _ -> return `Failure

(* other stages ***************************************************************)

let notify_failure context () =
  lwt _ =
    context.message_supervisor
      ~subject:"Sanity check failure"
      ~content:[
        pcdata "Hi," ; br () ;
        br () ;
        pcdata "The Sanity Check has failed. Please investigate"
      ] in
  return `None

let absorb_success context () =
  context.log_info "success!" ;
  return `None

(* the playbook ***************************************************************)

PLAYBOOK

 *test_api_member_functions ~> `Failure ~> notify_failure
  test_api_member_functions ~> `Success ~> absorb_success


(* the crontabs ***************************************************************)

CRON test_api_member_functions "0 * * * * *"
