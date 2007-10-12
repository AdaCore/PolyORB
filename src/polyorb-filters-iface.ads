------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . F I L T E R S . I F A C E                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
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

--  Messages exchanged by Filter components.

with Ada.Streams;

with PolyORB.Binding_Objects;
with PolyORB.Buffers;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.Types;

package PolyORB.Filters.Iface is

   pragma Elaborate_Body;

   use Ada.Streams;

   use PolyORB.Buffers;
   use PolyORB.Components;

   ---------------------
   -- Filter messages --
   ---------------------

   type Root_Data_Unit is abstract new Message with null record;
   subtype Data_Unit is Root_Data_Unit'Class;

   type Set_Server is new Root_Data_Unit with record
      Server         : Components.Component_Access;
      Binding_Object : Binding_Objects.Binding_Object_Access;
   end record;
   --  Direction: from lower to upper.
   --  Semantics: inform stacks participants of the ORB
   --  components they are working for.

   type Set_Target_Object is new Root_Data_Unit with record
      Target : PolyORB.Types.String;
   end record;
   --  Direction: from lower to upper.
   --  Semantics: a lower layer has determined what application
   --  object a specific message is destined to, and informs
   --  the upper layer.

   type Connect_Indication is new Root_Data_Unit with null record;
   --  Direction: from lower to upper.
   --  Semantics: a new incoming transport connection is
   --  being initiated.

   type Connect_Confirmation is new Root_Data_Unit with null record;
   --  Direction: from lower to upper.
   --  Semantics: a new client transport connection has been established.

   type Disconnect_Indication is new Root_Data_Unit with record
      Error : Errors.Error_Container;
   end record;
   --  Direction: from lower to upper.
   --  Semantics: a transport endpoint has been closed, or some other condition
   --  occured, causing the ORB to determine that the protocol layer must be
   --  dismantled. If the cause of the disconnect is a detected error, it is
   --  identified by the Error component.

   type Disconnect_Request is new Root_Data_Unit with null record;
   --  Direction: from upper to lower.
   --  Semantics: the application requests that the whole
   --    protocol stack be dismantled.

   type Data_Expected is new Root_Data_Unit with record
      --  Direction: from upper to lower.
      --  Semantics: prepare for reception of a message.

      In_Buf : Buffer_Access;
      --  Where to store the data when it arrives.

      Max : Stream_Element_Count;
      --  The maximum amount of data to be received.
   end record;

   type Data_Indication is new Root_Data_Unit with record
      Data_Amount : Stream_Element_Count := 0;
      --  The amount of data received, 0 if unknown.
   end record;
   --  Direction: from lower to upper.
   --  Semantics: Data has been received and must be handled.

   type Data_Out is new Root_Data_Unit with record
      --  Direction: from upper to lower.
      --  Semantics: send data out.

      Out_Buf : Buffer_Access;
      --  The data to be sent down.
   end record;

   type Filter_Error is new Root_Data_Unit with record
      --  Direction: from lower to upper.
      --  Semantics: an error in the transport or filtering layers
      --  has occured.

      Error : Errors.Error_Container;
   end record;

   type Check_Validity is new Root_Data_Unit with null record;
   --  Direction: from upper to lower
   --  Semantics: test whether protocol stack is usable to send a request.
   --  If so, an Empty message is returned, if not, a Filter_Error.

   ---------------------
   -- Helper routines --
   ---------------------

   procedure Expect_Data
     (Self   : access Filter'Class;
      In_Buf : Buffers.Buffer_Access;
      Max    : Ada.Streams.Stream_Element_Count);
   --  Signal Lower (Self) that data is expected using
   --  Data_Expected message.

end PolyORB.Filters.Iface;
