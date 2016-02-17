(*
 * diy_velo_bamblou
 *
 * ce playbook permet à un groupe de personnes de construire leur propre vélo
 * en bambou
 *
 * william@accret.io
 *
 *)

open Lwt

open Printf
open CalendarLib
open Calendar.Precise

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers
open Toolbox

let author = "william@accret.io"
let name = "Vélo en bambou"
let description = "Ce playbook permet à un groupe de personne de construire leur propre vélo en bambou"

let version = 0

(* the stages *****************************************************************)

let tag_leader = "leader"

let start context () =
  lwt leaders = context.search_members ~query:tag_leader () in
  let run_id = new_run_id () in
  lwt _ =
    context.message_supervisor
      ~subject:"Nouvelle série de vélos en bambous"
      ~data:(data_run_id run_id)
      ~content:[
        pcdata "Bonjour," ; br () ;
        br () ;
        pcdata "A qui voulez vous proposer de construire un vélo en bambou?"; br () ;
        br () ;
        pcdata "Envoyez moi autant d'emails que vous voulez dans le corps de ce message et je prendrai contact avec chacune des personnes. Vous pouvez aussi m'envoyer une pièce jointe que je forwarderai à chaque personne." ; br () ;
        br () ;
      ]
      ()
  in
  return `None

let tag_candidate = sprintf "candidate%Ld"
let key_original_email = sprintf "original-email-%Ld"

let extract_all_emails context message =
  match_lwt run_id_from_message context message with
    None ->
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"Can't extract candidates"
        ~content:[ pcdata "Couldn't find a run id" ]
        ()
    in
    return `None
  | Some run_id ->
    lwt members = extract_and_create_all_members_from_message context message in
    lwt members = Lwt_list.filter_s (fun member -> lwt is_member = context.is_member ~member in return (not is_member)) members in
    context.log_info "found %d new members" (List.length members) ;
    match members with
      [] -> return `None
    | _ as members ->
      lwt _ =
        Lwt_list.iter_s
          (fun member -> context.tag_member ~member ~tags:[ tag_candidate run_id ])
          members
      in
      lwt _ =
        context.reply_to
          ~message
          ~content:[
            pcdata "Merci, je vais prendre contact avec ces personnes!"
          ]
        ()
      in
      lwt _ =
        match_lwt $message(message)->attachments with
          [] -> return_unit
        | _ -> context.set ~key:(key_original_email run_id) ~value:(Ys_uid.to_string message) in
      return (`AskCandidatesForTheirOpinion run_id)

let tag_interested = sprintf "interested%Ld"
let tag_not_interested = sprintf "notinterested%Ld"

let send_summary context message =
  match_lwt run_id_from_message context message with
    None -> return `None
  | Some run_id ->
    lwt candidates = context.search_members ~query:(tag_candidate run_id) () in
    lwt interested = context.search_members ~query:(tag_interested run_id) () in
    lwt _ =
      context.reply_to
        ~message
        ~data:(data_run_id run_id)
        ~content:[
          pcdata "Pour l'instant il y a " ; pcdata (string_of_int (List.length interested)) ; pcdata " personnes intéressées sur " ; pcdata (string_of_int (List.length candidates)) ; pcdata " personnes contactées."
        ]
        ()
    in
    return `None

let tag_already_asked = sprintf "alreadyasked%Ld"

let ask_candidates_for_their_opinion context run_id =
  lwt candidates = context.search_members ~query:(sprintf "%s -%s" (tag_candidate run_id) (tag_already_asked run_id)) () in
  lwt attachments =
    match_lwt context.get ~key:(key_original_email run_id) with
      None -> return []
    | Some message ->
      let message = Ys_uid.of_string message in
      $message(message)->attachments
  in
  lwt _ =
    Lwt_list.iter_s
      (fun member ->
         lwt salutations = salutations_fr member in
         lwt _ =
           context.message_member
             ~member
             ~subject:"Construire un vélo en bambou sur mesure"
             ~data:(data_run_id run_id)
             ~attachments
             ~content:[
               salutations ; br () ;
               br () ;
               pcdata "Savez vous que l'on peut fabriquer un cadre de vélo à partir de bambou?" ; br () ;
               br () ;
               pcdata "Maintenant que j'ai construit le mien j'aimerais partager cette experience avec d'autres personnes en organisant une commande groupée des matériaux et un atelier de montage." ; br () ;
               ul [
                 li [ b [ pcdata "Où?" ] ; pcdata " ça se passerait à Paris, dans les locaux du Langevinium" ] ;
                 li [ b [ pcdata "Combien?" ] ; pcdata " le cadre seul (à prix coûtant) devrait revenir à environ 90 euros" ] ;
                 li [ b [ pcdata "Quand?" ] ; pcdata " à discuter selon les disponibilités des personnes intéréssées" ] ;
               ] ;
               pcdata "Est ce que ça vous tenterait? Aucun engagement pour le moment, dites moi juste si vous voulez rester dans la boucle et j'essaierai d'organiser tout ça en fonction des réponses." ; br () ;
             ]
             ()
         in
         lwt _ = context.tag_member ~member ~tags:[ tag_already_asked run_id ] in
         return_unit)
      candidates
  in
  return `None

let mark_interested context message =
  match_lwt run_id_from_message context message with
    None -> return `None
  | Some run_id ->
    lwt member = context.get_message_sender ~message in
    lwt _ = context.tag_member ~member ~tags:[ tag_interested run_id ] in
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Merci, j'attends d'avoir toutes les réponses et je vous recontacte!" ]
        ()
    in
    return `None

let mark_not_interested context message =
  match_lwt run_id_from_message context message with
    None ->
    return `None
  | Some run_id ->
    lwt member = context.get_message_sender ~message in
    lwt _ = context.tag_member ~member ~tags:[ tag_not_interested run_id ] in
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Ok, merci tout de même pour votre réponse!" ]
        ()
    in
    return `None


(* the flow *******************************************************************)

PLAYBOOK

*start<forward> ~> `Message of email ~> extract_all_emails ~> `AskCandidatesForTheirOpinion of int64 ~> ask_candidates_for_their_opinion
 start ~> `SendSummary of email ~> send_summary

 ask_candidates_for_their_opinion ~> `NotInterested of email ~> mark_not_interested
 ask_candidates_for_their_opinion ~> `Interested of email ~> mark_interested
