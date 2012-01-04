------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . F I L T E R S . I F A C E                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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
