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

let convert_from_b64 data =
  try_lwt

    let temp_file = Filename.temp_file "temp" ".b64" in
    let result_file = Filename.temp_file ~temp_dir:(Ys_config.get_string Ys_config.upload_directory) "image_" ".png" in
    lwt _ =
      Lwt_io.with_file ~flags:[ Unix.O_CREAT ; Unix.O_WRONLY ]
        ~perm:0o640
        ~mode:Lwt_io.output
        temp_file
        (fun ch -> Lwt_io.write ch data) in
    let cmd = Lwt_process.shell (Printf.sprintf "convert inline:%s %s" temp_file result_file) in

    match_lwt Lwt_process.exec cmd with
    | Unix.WEXITED _ ->
      lwt _ = Lwt_unix.unlink temp_file in
      Lwt_log.ign_info_f "file %s successfully converted to %s" temp_file result_file ;
      return (Some result_file)
    | _ ->
      Lwt_log.ign_info_f "converting file %s to a png failed" temp_file ;
      return_none

  with exn -> Lwt_log.ign_info_f "a convert job failed with exn %s" (Printexc.to_string exn); return_none
