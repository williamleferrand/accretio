(*
 * accretio grooming
 *
 * this playbook iterates over the db and asks missing information to the relevant users
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
let name = "Accretio grooming"
let description = "Iterates over the database and ask for missing names"

let version = 0
let tags = ""

(* some helpers ***************************************************************)

let key_last_asked = sprintf "last-asked-%d"
let tag_already_asked = "alreadyasked"
let tag_force_ask = "force"

(* the stages *****************************************************************)

let search_for_missing_names context () =
  context.log_info "iterating over all the users and ask for missing names" ;
  lwt _ =
    Object_member.Store.fold_flat_lwt
      (fun _ member ->
         lwt _ = context.add_member ~member in
         lwt name, preferred_email = $member(member)->(name, preferred_email) in
         context.log_info "inspecting member %d, email:%s name:%s" member name preferred_email ;
         lwt has_interacted_with_accretio =
           match_lwt $member(member)->messages with
             [] -> return_false
           | _ -> return_true
         in
         let has_blank_name =
           match name with
             "" -> true
           | _ -> false
         in
         lwt is_forced = context.check_tag_member ~member ~tag:tag_force_ask in
         lwt was_already_asked = context.check_tag_member ~member ~tag:tag_already_asked in
         let should_ask_for_name = is_forced || (has_blank_name && has_interacted_with_accretio && not was_already_asked) in

         if not should_ask_for_name then
           return (Some ())
         else
           begin
             lwt _ =
               match was_already_asked with
                 false ->
                 context.message_member
                   ~member
                   ~subject:"Quick question"
                   ~content:[
                     pcdata "Hi," ; br () ;
                     br () ;
                     pcdata "I just noticed that you are a member of Accretio, but there is no name associated with your account. ";
                     pcdata "How do you want to be called by in the future?"; br ()
                   ]
                   ()
               | true ->
                 context.message_member
                   ~member
                   ~subject:"Quick question"
                   ~content:[
                     pcdata "Hi," ; br () ;
                     br () ;
                     pcdata "I'm not sure I heard back from you about how you want to be called on Accretio. ";
                     pcdata "Do you want to pick up a name?"; br ()
                   ]
                   ()
             in
             lwt _ = context.untag_member ~member ~tags:[ tag_force_ask ] in
             lwt _ = context.tag_member ~member ~tags:[ tag_already_asked ] in
             lwt _ = context.set ~key:(key_last_asked member) ~value:(Int64.to_string (Ys_time.now ())) in
             return (Some ())
           end)
      ()
      None
      (-1)
  in
  return `None

let scrub_name context message =
  lwt _ =
    context.forward_to_supervisor
      ~message
      ~subject:"Please scrub the name"
      ~content:[ pcdata "Please scrub the name below" ]
      ()
  in
  return `None

let update_name context message =
  lwt name = context.get_message_content ~message in
  lwt original_message = context.get_original_message ~message in
  lwt member = context.get_message_sender ~message in
  context.log_info "updating member's %d name to %s" member name ;
  lwt _ = $member(member)<-name %% (fun _ -> name) in
  return (`ThankUser member)

let thank_user context member =
  context.log_info "thanking member %d" member ;
  lwt _ =
    context.message_member
      ~member
      ~subject:"Thanks!"
      ~content:[
        pcdata "Thanks, I've updated your name!"
      ]
      ()
  in
  return `None

(* the playbook ***************************************************************)

PLAYBOOK

    *search_for_missing_names<forward> ~> `Message of email ~> scrub_name ~> `Message of email ~> update_name ~> `ThankUser of int ~> thank_user
