------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               T E S T _ C L I E N T _ S E R V E R _ P K G                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005-2007, Free Software Foundation, Inc.       --
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

with Ada.Real_Time; use Ada.Real_Time;
with Echo;
with Echo.Impl;
with PortableServer.POA.Helper;
with PortableServer.POAManager;
with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with Ada.Exceptions;

package body Test_Client_Server_Pkg is

   The_IOR : Data;
   Initialized : Boolean := False;

   ------------
   -- Client --
   ------------

   procedure  Client is
      Sent_Msg : CORBA.String;

   begin
      while not Initialized
      loop
         delay until Clock + Milliseconds (100);
      end loop;

      --  Getting the CORBA.Object

      declare
         myecho : Echo.Ref;
      begin
         CORBA.ORB.String_To_Object
           (CORBA.To_CORBA_String (The_IOR.Str (1 .. The_IOR.Last)), myecho);
         Put_Line ("Client: Got the reference");
         Put_Line (The_IOR.Str (1 .. The_IOR.Last));

         --  Checking if it worked

         if Echo.Is_Nil (myecho) then
            Put_Line ("main : cannot invoke on a nil reference");
         end if;

         --  Sending message

         Sent_Msg := CORBA.To_CORBA_String (Standard.String'("Hello Ada !"));
         Put_Line ("I said : " & CORBA.To_Standard_String (Sent_Msg));

         for I in 1 .. 3 loop
            Put_Line (I'Img);
            Echo.printString (myecho, Sent_Msg);
            Put_Line ("print_String is Invoked!");
         end loop;
      end;

   end Client;

   ------------
   -- Server --
   ------------

   procedure Server is
   begin
      CORBA.ORB.Initialize ("ORB");
      Put_Line ("ORB : initialized");

      declare
         Root_POA : PortableServer.POA.Ref;
         Ref : CORBA.Object.Ref;
         Obj : constant CORBA.Impl.Object_Ptr
           := new Echo.Impl.Object;
      begin
         --  Retrieve Root POA

         Root_POA := PortableServer.POA.Helper.To_Local_Ref
           (CORBA.ORB.Resolve_Initial_References
            (CORBA.ORB.To_CORBA_String ("RootPOA")));

         PortableServer.POAManager.Activate
           (PortableServer.POA.Get_The_POAManager (Root_POA));

         --  Set up new object

         Ref := PortableServer.POA.Servant_To_Reference
           (Root_POA,  PortableServer.Servant (Obj));

         --  Send the IOR

         declare
            Tmp : String := CORBA.To_Standard_String
              (CORBA.Object.Object_To_String (Ref));
         begin

            The_IOR.Last := Tmp'Length;
            The_IOR.Str (1 .. Tmp'Length)  := Tmp (Tmp'First .. Tmp'Last);
         end;
      end;

      Put_Line ("running the ORB");
      Initialized := True;
      CORBA.ORB.Run;
   end Server;

begin
   null;
exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Information (E));

end Test_Client_Server_Pkg;
