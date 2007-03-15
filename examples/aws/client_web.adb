------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           C L I E N T _ W E B                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with AWS.Client;
with AWS.Response;
with AWS.Server;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

procedure Client_Web is

begin
   Put_Line ("client: initializing PolyORB");
   AWS.Server.Initialization;
   Put_Line ("client: initialized");

   if Argument_Count < 1 then
      Put_Line ("usage : client <URI_string_from_server>");
      return;
   else
      declare
         use AWS.Client;
         use AWS.Response;

         Connection : HTTP_Connection;
         Res : AWS.Response.Data;
      begin
         Create (Connection, Ada.Command_Line.Argument (1));
         Get (Connection, Res, Ada.Command_Line.Argument (1)
              & "?Mesg=Hello, Web world!");
         Close (Connection);
         Put_Line ("Client: sent Web request to "
                   & Ada.Command_Line.Argument (1));
         Put_Line ("Client: the server answered "
                   & AWS.Response.Message_Body (Res));
      end;
   end if;

end Client_Web;
