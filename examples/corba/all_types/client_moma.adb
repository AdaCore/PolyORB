------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          C L I E N T _ M O M A                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Testing MOMA client, interaction with ORB server 'all_types'.

with Ada.Command_Line;
with Ada.Text_IO;

with PolyORB.Initialization;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);
--  XXX this package should be renamed to PolyORB.Setup.Thread_Pool_Node ...

with MOMA.Message_Producers;

with MOMA.Messages;
with MOMA.Messages.MExecutes;
with MOMA.Types;

procedure Client_MOMA is

   use Ada.Command_Line;
   use Ada.Text_IO;

   use MOMA.Message_Producers;
   use MOMA.Messages;
   use MOMA.Messages.MExecutes;
   use MOMA.Types;

   MOMA2ORB_Producer : MOMA.Message_Producers.Message_Producer;

   --------------------------
   -- Execute Message Test --
   --------------------------

   procedure Test_MExecute;

   procedure Test_MExecute is
      MExecute_Message_Sent : MOMA.Messages.MExecutes.MExecute;

      Method_Name   : Map_Element;
      Return_1      : Map_Element;
      Arg_1         : Map_Element;
      Parameter_Map : Map;

   begin
      --  Create new Text Message
      Put_Line ("Formatting message.");
      MExecute_Message_Sent := Create_Execute_Message;
      Method_Name := (Name  => To_MOMA_String ("method"),
                      Value => To_Any (To_MOMA_String ("echoString")));

      Return_1 := (Name  => To_MOMA_String ("return_1"),
                   Value => To_Any (To_MOMA_String ("")));

      Arg_1 := (Name  => To_MOMA_String ("arg_1"),
                Value => To_Any (To_MOMA_String ("Hi Mom !")));

      Append (Parameter_Map, Method_Name);
      Append (Parameter_Map, Return_1);
      Append (Parameter_Map, Arg_1);

      Set_Parameter (MExecute_Message_Sent, Parameter_Map);

      Put_Line ("Sending message.");

      Send (MOMA2ORB_Producer, MExecute_Message_Sent);

      Put_Line ("Message sent.");

   end Test_MExecute;

   --------------------
   -- Main procedure --
   --------------------

begin
   PolyORB.Initialization.Initialize_World;
   --  Argument check.
   if Argument_Count /= 2 then
      Put_Line ("usage : client_moma <IOR_string_from_orb_server> \");
      Put_Line ("             <IOR_string_from_moma_server>");
      return;
   end if;

   --  Create Message Producer associated to the ORB object.
   MOMA2ORB_Producer := Create_Producer (To_MOMA_String (Argument (1)),
                                         To_MOMA_String (Argument (2)));

   --  Testing MExecute.
   Test_MExecute;

end Client_MOMA;
