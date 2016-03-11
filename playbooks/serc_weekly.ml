open Lwt

open Printf
open CalendarLib

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers

let author = "william@accret.io"
let name = "Swimmers circle"
let description = "Gather at aquatic park weekly for a friendly swim in the bay"

let version = 0
let tags = ""

let current_week () =
   let now = Calendar.now () in
   Calendar.year now * 53 + Calendar.week now

let create_weekly_invite_email context _ =
  context.log_info "creating weekly invite email" ;
  (* let's use the water temp api *)
  let open Noaa_t in
  let open Noaa_j in
  let uri_params = Netencoding.Url.mk_url_encoded_parameters
      [
        "date", "today" ;
        "station", "9414290" ; (* this is the crissey field station *)
        "product", "water_temperature" ;
        "units", "english" ;
        "time_zone", "gmt" ;
        "format", "json"
      ] in
  let uri = "/api/datagetter?" ^ uri_params in
  lwt noaa, _ = Ys_http.get ~max:1024000 ~host:"tidesandcurrents.noaa.gov" ~uri () in
  let temperatures = temperatureResults_of_string noaa in
  match List.rev temperatures.data with
  | [] -> return `None
  | recent :: _ -> return (`WaterTemperature recent.v)

let send_email_to_swimmers context temperature =
  let week = current_week () in
  lwt participants = context.search_members ~query:"active -suspended" () in
  Lwt_log.ign_info_f "found %d participants" (List.length participants) ;
  lwt _ =
    Lwt_list.iter_p
      (fun member ->
         lwt _ =
           context.message_member
             ~member
             ~subject:"Want to go swimming this Saturday?"
             ~content:[
               pcdata "Hi," ; br () ;
               br () ;
               pcdata "The water temperature at Aquatic Park is " ; pcdata temperature ; pcdata "°F, according to the NOAA API. " ;
               pcdata "Do you want to go test the waters this Saturday at 8am? We'll meet in front of the South End Rowing Club and hop in the sauna after a short swim :-)" ; br () ;
               br () ;
             pcdata "Let me know!" ; br () ;
               br () ;
               pcdata "Cheers," ;
               br () ;
               pcdata "William"
             ]
             () in return_unit)
      participants
  in
  return `None

let mark_participant tags_add tags_remove context message =
  lwt original = context.get_original_message ~message in
  context.log_info "message: %d, original message is %d" message original ;
  lwt member = context.get_message_sender ~message:original in
  let week = current_week () in
  lwt _ = context.tag_member ~member ~tags:[ Printf.sprintf "%s_%d" tags_add week ] in
  lwt _ = context.untag_member ~member ~tags:[ Printf.sprintf "%s_%d" tags_remove week ] in
  return `None

let mark_participant_as_not_joining context message = mark_participant "not_joining" "joining" context message

let mark_participant_as_joining context message = mark_participant "joining" "not_joining" context message


PLAYBOOK

  create_weekly_invite_email ~> `WaterTemperature of string ~> send_email_to_swimmers

      send_email_to_swimmers<simple_yes_no> ~> `No of email ~> mark_participant_as_not_joining
      send_email_to_swimmers<simple_yes_no> ~> `Yes of email ~> mark_participant_as_joining


CRON create_weekly_invite_email "0 0 * * 3 *"
