------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . F I L T E R S . I N T E R F A C E             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Messages exchanged by Filter components.

--  $Id$

with Ada.Streams; use Ada.Streams;

with PolyORB.Buffers; use PolyORB.Buffers;
with PolyORB.Components; use PolyORB.Components;

package PolyORB.Filters.Interface is

   -----------------------------
   -- Filter_Factory messages --
   -----------------------------

   type Create_Filter_Chain is new Message with null record;

   type Created_Filter_Chain is new Message with record
      Filter_Chain : Filter_Access;
   end record;

   ---------------------
   -- Filter messages --
   ---------------------

   type Root_Data_Unit is abstract new Message with null record;
   subtype Data_Unit is Root_Data_Unit'Class;

   type Set_Server is new Root_Data_Unit with record
      Server : Components.Component_Access;
   end record;
   --  Direction: from lower to upper.
   --  Semantics: inform stacks participants of the ORB
   --  component they are working for.

   type Set_Buffer is new Root_Data_Unit with record
      Buffer : Buffer_Access;
   end record;
   --  Direction: from upper to lower.
   --  Semantics: Buffer is to be used by filters along the
   --  chain to hold received data contents.

   type Connect_Indication is new Root_Data_Unit with null record;
   --  Direction: from lower to upper.
   --  Semantics: a new incoming transport connection is
   --  being initiated.

   type Connect_Confirmation is new Root_Data_Unit with null record;
   --  Direction: from lower to upper.
   --  Semantics: a new client transport connection has been established.

   type Disconnect_Indication is new Root_Data_Unit with null record;
   --  Direction: from lower to upper.
   --  Semantics: a transport endpoint has been closed.
   --    upper layers must release all associated resources.

   type Data_Indication is new Root_Data_Unit with null record;
   --  Direction: from lower to upper.
   --  Semantics: Data has been received and must be handled.

   type Data_Expected is new Root_Data_Unit with record
      --  Direction: from upper to lower.
      --  Semantics: prepare for reception of a message.

      In_Buf : Buffer_Access;
      --  Where to store the data when it arrives.

      Max : Stream_Element_Count;
      --  The maximum amount of data to be received.
   end record;

   type Data_Out is new Root_Data_Unit with record
      --  Direction: from upper to lower.
      --  Semantics: send data out.

      Out_Buf : Buffer_Access;
      --  The data to be sent down.
   end record;

end PolyORB.Filters.Interface;

