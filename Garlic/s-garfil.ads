------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . F I L T E R S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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
with System.Garlic.Heart;
with System.Garlic.Streams;
with System.Garlic.Types;
with System.Garlic.Utils;

package System.Garlic.Filters is

   pragma Elaborate_Body;

   procedure Filter_Incoming
      (Partition : in Types.Partition_ID;
       Opcode    : in System.Garlic.Heart.Any_Opcode;
       Stream    : in Streams.Stream_Element_Access;
       Offset    : in Ada.Streams.Stream_Element_Offset;
       Result    : out Streams.Stream_Element_Access;
       Error     : in out Utils.Error_Type);

   procedure Filter_Outgoing
      (Partition : in     Types.Partition_ID;
       Opcode    : in     System.Garlic.Heart.Any_Opcode;
       Stream    : access Streams.Params_Stream_Type;
       Result    : out    Streams.Stream_Element_Access;
       Error     : in out Utils.Error_Type);

   procedure Initialize;
   --  Elaboration code

   procedure Set_Channel_Filter (Partition, Filter : in String);
   --  The current partition uses Filter to communication with any
   --  partition of name Partition.

   procedure Set_Default_Filter (Filter : in String);
   --  When a partition has no communication filter yet, the default
   --  filter Filter is used.

   procedure Set_Registration_Filter (Filter : in String);
   --  During the first data exchange between two partitions, a
   --  registration filter is used to filter this first communication.

private

   type Filter_Type   is abstract tagged limited null record;
   type Filter_Access is access all Filter_Type'Class;

   type Filter_Params_Type is abstract tagged null record;
   type Filter_Params_Access is access all Filter_Params_Type'Class;

   function Filter_Incoming
      (Filter : in Filter_Type;
       Params : in Filter_Params_Access;
       Stream : in Streams.Stream_Element_Access;
       Offset : in Ada.Streams.Stream_Element_Offset)
      return Streams.Stream_Element_Access is abstract;

   function Filter_Outgoing
      (Filter : in     Filter_Type;
       Params : in     Filter_Params_Access;
       Stream : access Streams.Params_Stream_Type)
      return Streams.Stream_Element_Access is abstract;

   function Filter_Params_Read
      (Filter : Filter_Type;
       Stream : Ada.Streams.Stream_Element_Array)
      return Filter_Params_Access is abstract;

   function Filter_Params_Write
      (Filter : Filter_Type;
       Params : Filter_Params_Access)
      return Streams.Stream_Element_Access is abstract;

   procedure Free is new Ada.Unchecked_Deallocation
     (Filter_Params_Type'Class, Filter_Params_Access);

   procedure Generate_Params
      (Filter          : in  Filter_Type;
       Public_Params   : out Filter_Params_Access;
       Private_Params  : out Filter_Params_Access;
       Exchange_Params : out Boolean) is abstract;

   procedure Register_Filter
     (Filter : in Filter_Access;
      Name   : in String);

end System.Garlic.Filters;
