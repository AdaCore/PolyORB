------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--            S Y S T E M . G A R L I C . F I L T E R S . Z I P             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
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
with System.Garlic.Streams;

package System.Garlic.Filters.Zip is

   procedure Initialize;

private

   pragma Linker_Options ("-lz");

   type Compress_Filter_Type is new Filter_Type with null record;

   type Compress_Filter_Params_Type is new Filter_Params_Type with null record;

   function Filter_Incoming
     (Filter : Compress_Filter_Type;
      Params : Filter_Params_Access;
      Stream : Streams.Stream_Element_Access;
      Offset : Ada.Streams.Stream_Element_Offset)
     return Streams.Stream_Element_Access;

   function Filter_Outgoing
     (Filter : Compress_Filter_Type;
      Params : Filter_Params_Access;
      Stream : access Streams.Params_Stream_Type)
     return Streams.Stream_Element_Access;

   function Filter_Params_Read
     (Filter : Compress_Filter_Type;
      Stream : Ada.Streams.Stream_Element_Array)
     return Filter_Params_Access;

   function Filter_Params_Write
     (Filter : Compress_Filter_Type;
      Params : Filter_Params_Access)
     return Streams.Stream_Element_Access;

   procedure Generate_Params
     (Filter          : Compress_Filter_Type;
      Public_Params   : out Filter_Params_Access;
      Private_Params  : out Filter_Params_Access;
      Exchange_Params : out Boolean);

end System.Garlic.Filters.Zip;
