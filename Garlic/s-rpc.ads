------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--                           S Y S T E M . R P C                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            1.7                             --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GARLIC is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------
with Ada.Streams;
with System.Storage_Elements;

package System.RPC is

   type Partition_ID is range 0 .. 63;

   Communication_Error : exception;

   type Params_Stream_Type
     (Initial_Size : Ada.Streams.Stream_Element_Count) is new
       Ada.Streams.Root_Stream_Type with private;

   procedure Read
     (Stream : in out Params_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in out Params_Stream_Type;
      Item   : in Ada.Streams.Stream_Element_Array);

   --  Synchronous call

   procedure Do_RPC
     (Partition  : in Partition_ID;
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type);

   --  Asynchronous call

   procedure Do_APC
     (Partition  : in Partition_ID;
      Params     : access Params_Stream_Type);

   --  The handler for incoming RPCs.

   type RPC_Receiver is
     access procedure
       (Params     : access Params_Stream_Type;
        Result     : access Params_Stream_Type);

   procedure Establish_RPC_Receiver (
      Partition : in Partition_ID;
      Receiver  : in RPC_Receiver);

private

   pragma Inline (Read);
   pragma Inline (Write);

   type Node (<>);
   type Node_Ptr is access Node;

   type Node (Size : Ada.Streams.Stream_Element_Count) is record
      Content : Ada.Streams.Stream_Element_Array (1 .. Size);
      Current : Ada.Streams.Stream_Element_Offset := 1;
      Last    : Ada.Streams.Stream_Element_Offset := 1;
      Next    : Node_Ptr;
   end record;

   type Params_Stream_Type (Initial_Size : Ada.Streams.Stream_Element_Count) is
     new Ada.Streams.Root_Stream_Type with record
        First         : Node_Ptr := new Node (Initial_Size);
        Current       : Node_Ptr;
        Special_First : Boolean := False;
        Count         : Ada.Streams.Stream_Element_Count := 0;
     end record;

end System.RPC;

