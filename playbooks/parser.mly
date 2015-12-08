/*
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
 *
 */

%start breadmaking_leader_response, breadmaking_member_response, breadmaking_admin, r00t_leader_response, breadmaking_leader_message, breadmaking_member_message
%type < [ `Yes | `No ] > breadmaking_leader_response
%type < [ `NoOp | `Run of string ] > breadmaking_admin
%type < [ `Yes | `No ] > breadmaking_member_response
%type < int -> [> `Run of int * string | `None | `CreateAndAssignContext of int * int * string
                | `CreateContext of int * int | `Summarize of int
                | `Activate of int * string | `Pause of int * string ] > r00t_leader_response

%type < int -> [> `Summarize of int option | `MakeLeaderUnavailable of int | `MakeLeaderAvailable of int | `CantParseMessage of int
               | `MembersAreNotInterested of int | `MembersAreInterested of int | `Invite of int ] > breadmaking_leader_message

%type < int -> [> `MembersAreNotInterested of int | `MembersAreInterested of int | `CantDecide of int ] > breadmaking_member_message

%token <string> IDENT
%token <int> INT
%token RUN EOF YES NO


// the procedures
%token SUMMARIZE

// specific tokens
%token CREATE_CONTEXT CREATE_AND_ASSIGN_CONTEXT ACTIVATE PAUSE AVAILABLE NOT INVITE

// library stuff
%start library_message_yes_no
%type < [ `Yes | `No | `Unknown ] > library_message_yes_no

%%

/* library ********************************************************************/

library_message_yes_no:
    | YES                                      { `Yes }
    | NO                                       { `No }
    | EOF                                      { `Unknown }

/* r00t ***********************************************************************/

r00t_leader_response:
| RUN CREATE_AND_ASSIGN_CONTEXT INT IDENT { fun uid -> `CreateAndAssignContext (uid, $3, $4) }
| RUN CREATE_CONTEXT INT                  { fun uid -> `CreateContext (uid, $3) }
| RUN SUMMARIZE                             { fun uid -> `Summarize uid }
| RUN ACTIVATE IDENT                        { fun uid -> `Activate (uid, $3) }
| RUN PAUSE IDENT                           { fun uid -> `Pause (uid, $3) }
| IDENT r00t_leader_response                { $2 }
| EOF                                       { fun uid -> `None }

/* breadmaking ****************************************************************/

breadmaking_leader_message:
| RUN AVAILABLE                                 { fun uid -> `MakeLeaderAvailable uid }
| RUN NOT AVAILABLE                             { fun uid -> `MakeLeaderUnvailable uid }
| RUN SUMMARIZE                                 { fun uid -> `Summarize (Some uid) }
| RUN PAUSE                                     { fun uid -> `Pause uid }
| RUN ACTIVATE                                  { fun uid -> `Activate uid }
| RUN YES                                       { fun uid -> `MembersAreInterested uid }
| RUN NO                                        { fun uid -> `MembersAreNotInterested uid }
| RUN INVITE                                    { fun uid -> `Invite uid }
| IDENT breadmaking_leader_message              { $2 }
| EOF                                           { fun uid -> `CantParseMessage uid }

breadmaking_member_message:
| YES                                       { fun uid -> `MembersAreInterested uid }
| NO                                        { fun uid -> `MembersAreNotInterested uid }
| IDENT breadmaking_member_message          { $2 }
| EOF                                       { fun uid -> `CantDecide uid }

breadmaking_leader_response:
| YES breadmaking_leader_response { `Yes }
| NO breadmaking_leader_response { `No }
| EOF { `No }
| IDENT breadmaking_leader_response { $2 }

breadmaking_member_response:
| YES breadmaking_member_response { `Yes }
| NO breadmaking_member_response { `No }
| EOF { `No }
| IDENT breadmaking_member_response { $2 }

breadmaking_admin:
| RUN IDENT { `Run $2 }
| EOF { `NoOp }
| IDENT breadmaking_admin { $2 }
| YES breadmaking_admin { $2 }
| NO breadmaking_admin { $2 }

;

%%
