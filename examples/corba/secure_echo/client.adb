------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--   echo client.

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with CORBA.ORB;

with Echo;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with PolyORB.Utils.Report;

with PortableServer.POA;
pragma Warnings (Off, PortableServer.POA);
with PolyORB.Setup.Secure_Client;
pragma Warnings (Off, PolyORB.Setup.Secure_Client);

with Ada.Characters.Handling;
with Ada.Streams;
with CORBA.Object;
with PolyORB.Binding_Data;
with PolyORB.Binding_Data_QoS;
with PolyORB.QoS.Tagged_Components;
with PolyORB.References;

procedure Client is

   use Ada.Command_Line;
   use Ada.Text_IO;
   use PolyORB.Utils.Report;

   use Ada.Characters.Handling;
   use Ada.Streams;

   task type Thread is
      entry Run (My_Ref : Echo.Ref);
   end Thread;

   procedure Put (Item : Ada.Streams.Stream_Element_Array);

   To_Hex_Digit : constant array (Stream_Element range 0 .. 15) of Character
     := (0  => '0',
         1  => '1',
         2  => '2',
         3  => '3',
         4  => '4',
         5  => '5',
         6  => '6',
         7  => '7',
         8  => '8',
         9  => '9',
         10 => 'A',
         11 => 'B',
         12 => 'C',
         13 => 'D',
         14 => 'E',
         15 => 'F');

   ---------
   -- Put --
   ---------

   procedure Put (Item : Ada.Streams.Stream_Element_Array) is
      First : Stream_Element_Offset := Item'First;
      Last  : Stream_Element_Offset;
      Tail  : Stream_Element_Offset := 0;

   begin
      while First <= Item'Last loop
         Last := First + 16;

         Put ("     ");

         if Last > Item'Last then
            Tail := Last - Item'Last;
            Last := Item'Last;
         end if;

         for J in First .. Last loop
            Put (' ');
            Put (To_Hex_Digit (Item (J) / 16));
            Put (To_Hex_Digit (Item (J) mod 16));
         end loop;

         for J in 1 .. Tail loop
            Put ("   ");
         end loop;

         Put ("   ");

         for J in First .. Last loop
            if Is_Graphic (Character'Val (Item (J))) then
               Put (Character'Val (Item (J)));

            else
               Put ('.');
            end if;
         end loop;

         New_Line;

         First := Last + 1;
      end loop;
   end Put;

   Calls   : Positive := 1;
   Threads : Positive := 1;

   task body Thread is
      Ref : Echo.Ref;
      Sent_Msg, Rcvd_Msg : CORBA.String;

   begin
      select
         accept Run (My_Ref : Echo.Ref) do
            Ref := My_Ref;
         end Run;

      or
         terminate;
      end select;

      for J in 1 .. Calls loop
         begin
            --  Sending message

            Sent_Msg :=
              CORBA.To_CORBA_String (Standard.String'("Hello Ada !"));
            Rcvd_Msg := Echo.echoString (Ref, Sent_Msg);

            --  Printing result

            Put_Line ("I said : " & CORBA.To_Standard_String (Sent_Msg));
            Put_Line
              ("The object answered : " & CORBA.To_Standard_String (Rcvd_Msg));

         exception
            when E : others =>
               Put_Line (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;
   end Thread;

begin
   New_Test ("Echo client");

   CORBA.ORB.Initialize ("ORB");

   if Argument_Count < 1 then
      Put_Line
        ("usage : client <IOR_string_from_server>"
           & " [<call count>] [<thread count>]");
      return;
   end if;

   if Argument_Count >= 2 then
      Calls := Positive'Value (Argument (2));
   end if;

   if Argument_Count >= 3 then
      Threads := Positive'Value (Argument (3));
   end if;

   declare
      myecho : Echo.Ref;
      Aux    : array (Positive range 1 .. Threads) of Thread;

   begin
      --  Getting the CORBA.Object

      CORBA.ORB.String_To_Object
        (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)), myecho);

      declare
         use PolyORB.Binding_Data;
         use PolyORB.Binding_Data_QoS;
         use PolyORB.QoS;
         use PolyORB.QoS.Tagged_Components;
         use PolyORB.QoS.Tagged_Components.GIOP_Tagged_Component_Lists;
         use PolyORB.References;

         Profiles : constant Profile_Array
           := Profiles_Of
           (CORBA.Object.Internals.To_PolyORB_Ref (CORBA.Object.Ref (myecho)));
         QoS      : QoS_GIOP_Tagged_Components_Parameter_Access;
         Iter     : Iterator;

      begin
         for J in Profiles'Range loop
            Put_Line (Profile_Tag'Image (Get_Profile_Tag (Profiles (J).all)));
            QoS :=
              QoS_GIOP_Tagged_Components_Parameter_Access
              (Get_Profile_QoS (Profiles (J), GIOP_Tagged_Components));

            if QoS /= null then
               Iter := First (QoS.Components);

               while not Last (Iter) loop
                  Put_Line (Component_Id'Image (Value (Iter).Tag));
                  Put (Value (Iter).Data.all);
                  Next (Iter);
               end loop;
            end if;
         end loop;
      end;

      --  Checking if it worked

      if Echo.Is_Nil (myecho) then
         Put_Line ("main : cannot invoke on a nil reference");
         return;
      end if;

      for J in Aux'Range loop
         Aux (J).Run (myecho);
      end loop;
   end;

   CORBA.ORB.Shutdown (False);

   End_Report;

exception
   when E : CORBA.Transient =>
      declare
         Memb : CORBA.System_Exception_Members;
      begin
         CORBA.Get_Members (E, Memb);
         Put ("received exception transient, minor");
         Put (CORBA.Unsigned_Long'Image (Memb.Minor));
         Put (", completion status: ");
         Put_Line (CORBA.Completion_Status'Image (Memb.Completed));

         End_Report;
      end;

      CORBA.ORB.Shutdown (False);

   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      CORBA.ORB.Shutdown (False);
end Client;
