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
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
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
with System.Garlic.Storage_Handling;
pragma Elaborate_All (System.Garlic.Storage_Handling);
with System.RPC;

package System.Garlic.Streams is

   package Streams_Pools is new Storage_Handling
     (Max_Objects        => 16,
      Static_Object_Size => 16_384);

   Streams_Pool : Streams_Pools.Garlic_Storage_Pool;

   type Stream_Element_Access is access Ada.Streams.Stream_Element_Array;
   for Stream_Element_Access'Storage_Pool use Streams_Pool;

   procedure Free is
      new Ada.Unchecked_Deallocation (Ada.Streams.Stream_Element_Array,
                                      Stream_Element_Access);

   function To_Stream_Element_Array
     (Params : access System.RPC.Params_Stream_Type;
      Unused : Ada.Streams.Stream_Element_Count := 0)
      return Ada.Streams.Stream_Element_Array;
   pragma Inline (To_Stream_Element_Array);
   --  This routine "looks" into the Params structure to extract the
   --  Stream_Element_Array which will be sent accross the network. It
   --  also let Unused places to sKtore extra information.

   function To_Stream_Element_Access
     (Params : access System.RPC.Params_Stream_Type;
      Unused : Ada.Streams.Stream_Element_Count := 0)
     return Stream_Element_Access;
   --  Same thing, but return a dynamically allocated pointer

   procedure To_Params_Stream_Type
     (Content : Ada.Streams.Stream_Element_Array;
      Params  : access System.RPC.Params_Stream_Type);
   pragma Inline (To_Params_Stream_Type);
   --  Other way

end System.Garlic.Streams;
