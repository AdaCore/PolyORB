------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          T E S T _ L S _ P K G                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ravenscar_Setup;
pragma Warnings (Off, Ravenscar_Setup);
pragma Elaborate_All (Ravenscar_Setup);

with PolyORB.ORB.Thread_Pool;
pragma Warnings (Off, PolyORB.ORB.Thread_Pool);
pragma Elaborate_All (PolyORB.ORB.Thread_Pool);

with PolyORB.ORB_Controller.Workers;
pragma Warnings (Off, PolyORB.ORB_Controller.Workers);
pragma Elaborate_All (PolyORB.ORB_Controller.Workers);

with PolyORB.Representations.CDR.Common;
use PolyORB.Representations.CDR.Common;

with PolyORB.Initialization; use PolyORB.Initialization;
with PolyORB.Local_Sockets;  use PolyORB.Local_Sockets;

with Ada.Text_IO;     use Ada.Text_IO;
with PolyORB.Buffers; use PolyORB.Buffers;
with Ada.Text_IO;

with Ada.Streams;
with PolyORB.Types;
with Ada.Real_Time; use Ada.Real_Time;

package body Test_LS_Pkg is
   Initialized : Boolean := False;

   task Server;
   task Client;

   ------------
   -- Server --
   ------------

   task body Server is
      Address : Local_Socket_Addr;
      Server  : constant Local_Socket_Access := new Local_Socket_Type;
      Socket  : constant Local_Socket_Access := new Local_Socket_Type;

   begin
      Put_Line ("Server : enter ");
      if not Initialized then
         PolyORB.Initialization.Initialize_World;
         Initialized := True;
      end if;

      Create_Socket (Server.all);

      Listen_Socket (Server.all);

      Accept_Socket (Server.all, Socket.all, Address);
      Put_Line
        ("Server : My connecting port is : " & Integer (Address.LPort)'Img);

      declare
         B    : constant Buffer_Access           := new Buffer_Type;
         Size : Ada.Streams.Stream_Element_Count :=
            Ada.Streams.Stream_Element_Count (4);
         S    : PolyORB.Types.Long;
      begin
         Put_Line ("Server : Expecting the client message");
         Read (Socket, B, Size);

         Rewind (B);
         S := Unmarshall (B);
         Put_Line ("Server : Received, " & S'Img);

      end;
   end Server;

   ------------
   -- Client --
   ------------

   task body Client is
      Address : Local_Socket_Addr;
      Socket  : constant  Local_Socket_Access := new Local_Socket_Type;

   begin
      Put_Line ("Client : enter ");
      delay until Clock + Milliseconds (2000);
      if not Initialized then
         PolyORB.Initialization.Initialize_World;
         Initialized := True;
      end if;

      Create_Socket (Socket.all);

      Address.LPort := 1;
      Put_Line ("Client, about to connect");
      Connect_Socket (Socket.all, Address);
      Put_Line ("Client,  Connected!");
      --  Send a string to the Server

      declare
         B : constant Buffer_Access := new Buffer_Type;
      begin
         Marshall (B, PolyORB.Types.Long (20050720));
         Put_Line ("about to send a Buffer of length" & Length (B)'Img);
         Write (Socket, B);
      end;
   end Client;

end Test_LS_Pkg;
