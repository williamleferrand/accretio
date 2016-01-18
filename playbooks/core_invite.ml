(*
 * core - invite
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

let has_already_declined = sprintf "core-invite-has-already-declined-%d"
let tag_timer_reminder = sprintf "core-invite-reminded-%d"

let invite context message =
  lwt content = context.get_message_content ~message in
  let emails = Ys_email.get_all_emails content in

  lwt already_members, already_declined, invited =
    Lwt_list.fold_left_s
      (fun (already_members, already_declined, invited) email ->
         context.log_info "inviting member with email %s to society %d" email context.society ;
         lwt member =
           match_lwt Object_member.Store.find_by_email email with
           | Some uid -> return uid
           | None ->
             match_lwt Object_member.Store.create
                         ~preferred_email:email
                         ~emails:[ email ]
                         () with
             | `Object_already_exists (_, uid) -> return uid
             | `Object_created member -> return member.Object_member.uid
         in
         match_lwt context.is_member ~member with
           true -> return ((member, email) :: already_members, already_declined, invited)
         | false ->
           (* check if the member hasn't declined already *)
           match_lwt context.get ~key:(has_already_declined member) with
             Some _ -> return (already_members, (member, email) :: already_declined, invited)
           | None ->
             lwt _ =
               context.message_member
                 ~member
                 ~subject:context.society_name
                 ~content:[
                   pcdata "Hi," ; br () ;
                   br () ;
                   pcdata "I'm running a group called " ; i [ pcdata context.society_name ] ; pcdata "."; br () ;
                   br () ;
                   pcdata context.society_description ; br ();
                   br () ;
                   pcdata "Would you be interested in joining us? No signup is necessary; we usually organize events simply via email." ; br () ;
                 ]
                 ()
             in
             lwt _ =
               context.set_timer
                 ~label:(tag_timer_reminder member)
                 ~duration:(Calendar.Period.lmake ~hour:26 ())
                 (`RemindMember member)
             in
             return (already_members, already_declined, ((member, email) :: invited)))
      ([], [], [])
      emails
  in

  lwt _ =
    context.reply_to
      ~message
      ~content:[
        pcdata "Great. Here is what I did:" ; br () ;
        br () ;
        pcdata "Already members:" ;
        ul (List.map (fun (_, email) -> li [ pcdata email ]) already_members) ;
        br () ;
        pcdata "Already declined:" ;
        ul (List.map (fun (_, email) -> li [ pcdata email ]) already_declined) ;
        br () ;
        pcdata "Invited:" ;
        ul (List.map (fun (_, email) -> li [ pcdata email ]) invited) ;
        br () ;
        pcdata "Let's see what comes back!"
      ]
      ()
  in

  return `None

let remind context member =
  context.log_info "sending reminder to member %d" member ;
  lwt _ =
    context.cancel_timers ~query:(tag_timer_reminder member)
  in
  lwt _ =
    context.message_member
      ~member
      ~subject:context.society_name
      ~content:[
        pcdata "Apologies for the resend, but I thought that you might have missed my previous message." ; br () ;
        br () ;
        pcdata "Would you be interested in joining our " ; i [ pcdata context.society_name ] ; pcdata " group?" ;
      ]
      ()
  in
  return `None

let accepted context message =
  lwt member = context.get_message_sender ~message in
  context.log_info "adding member %d to the society" member ;
  lwt _ = context.add_member ~member in
  lwt _ =
    context.cancel_timers ~query:(tag_timer_reminder member)
  in
  lwt _ =
    context.reply_to
      ~message
      ~content:[
        pcdata "Great thanks!" ; pcdata " I added you to the list of participants, stay tuned!" ; br ()
      ]
      ()
  in
  return `None

let declined context message =
  lwt member = context.get_message_sender ~message in
  context.log_info "removing member %d to the society" member ;
  lwt _ = context.remove_member ~member in
  lwt _ = context.cancel_timers ~query:(tag_timer_reminder member) in
  lwt _ = context.set ~key:(has_already_declined member) ~value:"true" in
  lwt _ =
    context.reply_to
      ~message
      ~content:[
        pcdata "Ok!" ; pcdata " If you change you mind later, don't hesitate to be get back in touch!" ; br ()
      ]
      ()
  in
  return `None


COMPONENT

                                    remind ~> `Declined of email ~> declined ~> `Accepted of email ~> accepted
  -invite ~> `RemindMember of int ~> remind ~> `Accepted of email ~> accepted
   invite ~> `Accepted of email ~> accepted
   invite ~> `Declined of email ~> declined
