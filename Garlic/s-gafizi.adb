------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--            S Y S T E M . G A R L I C . F I L T E R S . Z I P             --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Streams;           use Ada.Streams;
with Interfaces.C;
with System.Garlic.Filters;
pragma Warnings (Off, System.Garlic.Filters);
with System.Garlic.Debug; use System.Garlic.Debug;
with System.Garlic.Streams; use System.Garlic.Streams;

package body System.Garlic.Filters.Zip is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GAFIZI", "(s-gafizi): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   package C renames Interfaces.C;
   use C;

   function Compress (dest     : in System.Address;
                      dest_len : in System.Address;
                      src      : in System.Address;
                      src_len  : in C.long)
                      return C.int;
   pragma Import (C, Compress,   "compress");

   function Decompress (dest     : in System.Address;
                        dest_len : in System.Address;
                        src      : in System.Address;
                        src_len  : in C.long)
                        return C.int;
   pragma Import (C, Decompress, "uncompress");

   Compressor   : aliased Compress_Filter_Type;

   ---------------------
   -- Filter_Incoming --
   ---------------------

   function Filter_Incoming
      (Filter : in Compress_Filter_Type;
       Params : in Filter_Params_Access;
       Stream : in Ada.Streams.Stream_Element_Array)
      return Stream_Element_Access is
      Target_Length : Stream_Element_Offset := 0;
      Target_Buffer : Stream_Element_Access;
      target_bytes  : C.long;
      source_bytes  : C.long;
      result        : C.int;

   begin
      for I in reverse Stream'First .. Stream'First + 3 loop
         Target_Length :=
           Target_Length * 256 + Stream_Element_Offset (Stream (I));
      end loop;
      target_bytes := C.long (Target_Length);
      Target_Buffer := new Stream_Element_Array (1 .. Target_Length);
      if Target_Length > 0 then
         source_bytes := Stream'Length - 4;
         result := Decompress
           (Target_Buffer (Target_Buffer'First)'Address, target_bytes'Address,
            Stream (Stream'First + 4)'Address, source_bytes);
      end if;
      return Target_Buffer;
   end Filter_Incoming;

   ---------------------
   -- Filter_Outgoing --
   ---------------------

   function Filter_Outgoing
      (Filter : in     Compress_Filter_Type;
       Params : in     Filter_Params_Access;
       Stream : access Streams.Params_Stream_Type)
      return Stream_Element_Access is
      Source_Length : Stream_Element_Offset;
      Target_Length : Stream_Element_Offset;
      target_bytes,
      source_bytes  : C.long;
      Source_Buffer : Stream_Element_Access;
      Target_Buffer : Stream_Element_Access;
      result        : C.int;
      Result_Buffer : Stream_Element_Access;

   begin
      Source_Buffer := To_Stream_Element_Access (Stream);
      source_bytes  := Source_Buffer'Length;
      Source_Length := Stream_Element_Offset (source_bytes);

      target_bytes  := ((source_bytes + 12) * 11) / 10;
      Target_Length := Stream_Element_Offset (target_bytes);
      Target_Buffer := new Stream_Element_Array (1 .. Target_Length + 4);

      if source_bytes = 0 then
         target_bytes := 0;
      else
         result := Compress
           (Target_Buffer (5)'Address, target_bytes'Address,
            Source_Buffer (Source_Buffer'First)'Address, source_bytes);
      end if;
      Target_Length := Stream_Element_Offset (target_bytes);
      for I in Target_Buffer'First .. Target_Buffer'First + 3 loop
         Target_Buffer (I) := Stream_Element (Source_Length mod 256);
         Source_Length     := Source_Length / 256;
      end loop;
      Result_Buffer :=
       new Stream_Element_Array'(Target_Buffer (1 .. Target_Length + 4));
      Free (Source_Buffer);
      Free (Target_Buffer);
      return Result_Buffer;
   end Filter_Outgoing;

   ------------------------
   -- Filter_Params_Read --
   ------------------------

   function Filter_Params_Read
      (Filter : Compress_Filter_Type;
       Stream : Stream_Element_Array)
     return Filter_Params_Access is
   begin
      return null;
   end Filter_Params_Read;

   -------------------------
   -- Filter_Params_Write --
   -------------------------

   function Filter_Params_Write
      (Filter : Compress_Filter_Type;
       Params : Filter_Params_Access)
     return Stream_Element_Access is
   begin
      return null;
   end Filter_Params_Write;

   ---------------------
   -- Generate_Params --
   ---------------------

   procedure Generate_Params
      (Filter          : in  Compress_Filter_Type;
       Public_Params   : out Filter_Params_Access;
       Private_Params  : out Filter_Params_Access;
       Exchange_Params : out Boolean) is
   begin
      Public_Params   := null;
      Private_Params  := null;
      Exchange_Params := False;
   end Generate_Params;

begin
   Register_Filter (Compressor'Access, "zip");
end System.Garlic.Filters.Zip;
