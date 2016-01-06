(*
 * dinners with friends
 *
 * this playbook organizes dinners for a group of friends, asking them for
 * advice and booking restaurants
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
let name = "Dinners with friends"
let description = "This playbook organizes dinners for groups of friends. It randomly asks members for suggestions, collects opinions from the group and make reservations."

let version = 0

(* local parameters *)

let key_run_id = "dinner-with-friends-run-id"
let key_volunteer = sprintf "dinner-with-friends-volunteer-%Ld"
let tag_volunteer = sprintf "volunteer%Ld"
let tag_joining = sprintf "joining%Ld"
let key_date = "dinner-with-friend-date"

(* the stages *****************************************************************)

let schedule_dinner context () =
  lwt _ = context.set ~key:key_run_id ~value:(Int64.to_string (Ys_time.now ())) in
  lwt participants = context.search_members ~query:"active" () in
  let count_participants = List.length participants in
  match_lwt context.get ~key:"min-participants" with
    None -> context.log_warning "min-participants is missing" ; return `None
  | Some min_participants ->
    context.log_info
      "scheduling dinner, we have %d potential participants, min is %s"
      count_participants
      min_participants ;
    let min_participants = try int_of_string min_participants with _ -> 1 in
    if count_participants < min_participants then
      begin
        context.log_warning "not enough potential participants to set up something" ;
        return `None
      end
    else
      begin
        lwt _ =
          context.message_supervisor
            ~subject:"Please drop a line for the next organizer"
            ~content:[
              pcdata "Hi" ; br () ;
              br () ;
              pcdata "There are enough potential participants for another dinner. " ;
              pcdata "Please reply with a custom message that will be used to contact the next organizer."
            ] in
        return `None
    end


let no_volunteer context () =
  context.log_info "no volunteer found" ;
  return `None


let ask_volunteer_for_yelp_link context member =
  match_lwt context.get ~key:key_run_id with
    None -> return `None
  | Some run_id ->
    let run_id = Int64.of_string run_id in
    lwt _ = context.set ~key:(key_volunteer run_id) ~value:(string_of_int member) in
    lwt _ = context.tag_member ~member ~tags: [ tag_volunteer run_id ; tag_joining run_id ] in
    context.log_info "asking %d for a yelp link" member ;
    lwt _ =
      context.message_member
        ~member
        ~subject:"Yelp link?"
        ~content:[
          pcdata "Great, thanks!" ; br () ;
          br () ;
          pcdata "Where could we go? Just sent me a yelp link and I'll forward it to the group!" ; br ()
        ]
    in
  return `None


let review_yelp_link context message =
  lwt _ = context.forward_to_supervisor ~message ~subject:"Review the yelp link" in
  return `None


let forward_yelp_link_to_all_members context message =
  match_lwt context.get ~key:key_run_id with
    None -> return `None
  | Some run_id ->
    let run_id = Int64.of_string run_id in
    match_lwt context.get ~key:(key_volunteer run_id) with
      None -> return `None
    | Some volunteer ->
      match_lwt context.get ~key:(key_date) with
        None -> return `None
      | Some date ->
        let volunteer = int_of_string volunteer in
        lwt content = context.get_message_content ~message in
        lwt participants = context.search_members ~query:(sprintf "active -volunteer%Ld" run_id) () in
        lwt volunteer = $member(volunteer)->name in
        lwt _ =
          Lwt_list.iter_s
            (fun member ->
               context.message_member
                 ~member
                 ~subject:"Dinner with friends?"
                 ~content:[
                   pcdata "Hello!" ; br () ;
                   br () ;
                   pcdata volunteer ; pcdata " has a great suggestion for the next dinner: " ; br () ;
                   br () ;
                   Raw.a ~a:[ a_href (uri_of_string (fun () -> content)) ] [ pcdata content ] ; br () ;
                   br () ;
                   pcdata date ; br () ;
                   br () ;
                   pcdata "Are you in?" ; br () ;
                 ])
        participants
        in
        return `None


let mark_member_as_joining context message =
  match_lwt context.get ~key:key_run_id with
    None -> return `None
  | Some run_id ->
    let run_id = Int64.of_string run_id in
    lwt member = context.get_message_sender message in
    lwt _ = context.tag_member ~member ~tags:[ tag_joining run_id ] in
    return `None


let create_dashboard context () =
  match_lwt context.get ~key:key_run_id with
    None -> return `None
  | Some run_id ->
    let run_id = Int64.of_string run_id in
    lwt participants = context.search_members ~query:(tag_joining run_id) () in
    lwt emails =
      Lwt_list.map_p
        (fun member ->
           lwt email = $member(member)->preferred_email in
           return (li [ pcdata email ]))
      participants
   in
   lwt _ =
     context.message_supervisor
       ~subject:(sprintf "Dasbhoard for run %Ld" run_id)
       ~content:[
         pcdata "Hi," ; br () ;
         br () ;
         pcdata "Here are the registered participants:" ; br () ;
         br () ;
         ul emails  ;
         br () ;
       ] in
   return `None

let mark_sender_as_volunteer context message =
  lwt member = context.get_message_sender ~message in
  match_lwt context.get ~key:key_run_id with
    None -> return `None
  | Some run_id ->
    let run_id = Int64.of_string run_id in
    lwt _ = context.set ~key:(key_volunteer run_id) ~value:(string_of_int member) in
    lwt _ = context.tag_member ~member ~tags: [ tag_volunteer run_id ; tag_joining run_id ] in
    return (`Message message)


(* the playbook ***************************************************************)


PARAMETERS
  (* - "How often?", "period"
   - "Geographical area?", "geography" *)
   - "Minimum number of participants?", "min-participants"
   - "Maximum number of participants?", "max-participants"


PLAYBOOK

   #import find_volunteer

   *schedule_dinner<content> ~> `Content of string ~> find_volunteer_with_tagline
                                                      look_for_candidate ~> `NoVolunteer ~> no_volunteer
                                                      return_volunteer ~> `Volunteer of int ~> ask_volunteer_for_yelp_link<forward> ~> `Message of email ~> review_yelp_link<forward> ~> `Message of email ~> forward_yelp_link_to_all_members

                                                      candidate_response ~> `Message of int ~> mark_sender_as_volunteer ~> `Message of email ~> review_yelp_link


       forward_yelp_link_to_all_members ~> `Yes of email ~> mark_member_as_joining

 *create_dashboard

(* the cron part isn't easy as we want to make it dependent from the parameter above *)
