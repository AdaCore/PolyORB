------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . S T R E A M S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;
with Ada.Unchecked_Deallocation;
with System.Garlic.Debug;
with System.Garlic.Storage_Handling;
pragma Elaborate_All (System.Garlic.Storage_Handling);

package System.Garlic.Streams is

   --  This package defines types and utilities related to Stream handling.
   --  These types are not defined in System.Garlic.Types because they are
   --  way too large to be considered as basic types.

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
        First         : Node_Ptr;
        Current       : Node_Ptr;
        Special_First : Boolean := False;
        Count         : Ada.Streams.Stream_Element_Count := 0;
     end record;

   type Params_Stream_Access is access Params_Stream_Type;

   procedure Read
     (Stream : in out Params_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in out Params_Stream_Type;
      Item   : in Ada.Streams.Stream_Element_Array);

   pragma Inline (Read);
   pragma Inline (Write);

   package Streams_Pools is new Storage_Handling
     (Max_Objects        => 16,
      Static_Object_Size => 16_384);

   Streams_Pool : Streams_Pools.Garlic_Storage_Pool;

   type Stream_Element_Access is access Ada.Streams.Stream_Element_Array;
   for Stream_Element_Access'Storage_Pool use Streams_Pool;

   procedure Deep_Copy
     (Source : in out Params_Stream_Type;
      Target : access Params_Stream_Type);
   pragma Inline (Deep_Copy);
   --  Deep copy Source into Target and read the original packet. This is
   --  needed to be able to drop the Params_Stream_Type without losing its
   --  content.

   procedure Deallocate (Stream : in out Params_Stream_Access);
   pragma Inline (Deallocate);
   --  This procedure make sure that unconsumed data has been freed. This
   --  may occur in case of cancellation.

   procedure Dump
     (Level  : in System.Garlic.Debug.Debug_Level;
      Stream : in Ada.Streams.Stream_Element_Array;
      Key    : in System.Garlic.Debug.Debug_Key);
   --  Same as Print_Debug_info except that this procedure prints
   --  Stream content.

   procedure Free is
     new Ada.Unchecked_Deallocation
     (Ada.Streams.Stream_Element_Array, Stream_Element_Access);

   procedure To_Params_Stream_Type
     (Content : Ada.Streams.Stream_Element_Array;
      Params  : access Params_Stream_Type);
   pragma Inline (To_Params_Stream_Type);

   function To_Stream_Element_Access
     (Params : access Params_Stream_Type;
      Unused : Ada.Streams.Stream_Element_Count := 0)
     return Stream_Element_Access;

   function To_Stream_Element_Array
     (Params : access Params_Stream_Type;
      Unused : Ada.Streams.Stream_Element_Count := 0)
      return Ada.Streams.Stream_Element_Array;
   pragma Inline (To_Stream_Element_Array);
   --  This routine "looks" into the Params structure to extract the
   --  Stream_Element_Array which will be sent accross the network. It
   --  also let Unused places to store extra information.

   procedure Free is
     new Ada.Unchecked_Deallocation
     (Params_Stream_Type, Params_Stream_Access);

end System.Garlic.Streams;
