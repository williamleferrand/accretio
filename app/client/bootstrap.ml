(*
 * Accretio is an API, a sandbox and a runtime for social playbooks
 *
 * Copyright (C) 2015 William Le Ferrand
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


open Lwt
open Eliom_content.Html5

let init service pipe session fb_app_id stripe_key =
  (* the first step is to update the session, even before registering any handler *)
  Updater.start () ;
  Sessions.update_session session ;
  (* Sessions.register_pipe pipe ;
  Sessions.register_fb_app_id fb_app_id ; *)
  (* register the container *)
  Lwt.ignore_result
    ((* lwt _ = Ys_googlemaps.load () in
     Ys_stripe.load () ;
     lwt _ = Ys_stripe.set_publishable_key stripe_key in *)
     lwt _ = Eliom_client.wait_load_end () in
     Dom_html.window##scroll(0, 1) ;
     Manip.appendToBody (Nutshell.body ()) ;
     Manip.appendToBody (Footer.dom ()) ;
     Service.init () ;
     Service.goto service ;
     (* Ys_facebook.load !Sessions.fb_params ; *)
     return_unit)
