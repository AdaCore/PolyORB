------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--   echo client.
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with M1; use M1;
with M1.Echo;

procedure Client is
   Sent_Msg, Rcvd_Msg, IOR : CORBA.String;
   myecho : Echo.Ref;

begin

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   --  transforms the Ada string into CORBA.String
   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));

   --  getting the CORBA.Object
   CORBA.ORB.String_To_Object (IOR, myecho);

   --  checking if it worked
   if Echo.Is_Nil (myecho) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   --  sending message
   Sent_Msg := CORBA.To_CORBA_String (Standard.String'("Hello Ada !"));
   --  Sent_Msg := CORBA.To_CORBA_String (Standard.String'("Hello!"));
   Rcvd_Msg := Echo.echoMy_String (myecho, M1.My_String (Sent_Msg));

   --  printing result
   Put_Line ("I said : " & CORBA.To_Standard_String (Sent_Msg));
   Put_Line ("The object answered : " & CORBA.To_Standard_String (Rcvd_Msg));
exception
   when E : CORBA.Transient =>
      declare
         Memb : System_Exception_Members;
      begin
         Get_Members (E, Memb);
         Put ("received exception transient, minor");
         Put (Unsigned_Long'Image (Memb.Minor));
         Put (", completion status: ");
         Put_Line (Completion_Status'Image (Memb.Completed));
      end;
end Client;
