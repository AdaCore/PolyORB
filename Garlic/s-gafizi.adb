------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--          S Y S T E M . G A R L I C . F I L T E R S . Z I P               --
--                                                                          --
--                                 B o d y                                  --
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

-- This  package  is part of  the  transparent data filtering  extension to --
-- GARLIC  developed at  the  Software Engineering Laboratory of the  Swiss --
-- Federal Institute of Technology in Lausanne (EPFL).                      --

with System.Garlic.Filters;
pragma Elaborate (System.Garlic.Filters);

with System.Garlic.Debug;
with System.Garlic.Utils;
with System.RPC;
with Ada.Streams;
with Interfaces.C;

use Ada.Streams;

use System.Garlic.Debug;
use System.Garlic.Utils;
use System.RPC;

package body System.Garlic.Filters.Zip is

   pragma Linker_Options ("-lz");

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("FILTER", "(s-gafizi): ");

   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   package C renames Interfaces.C;

   use C;

   package Zipper is

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

   end Zipper;

   use Zipper;

   Compressor        : aliased Compress_Filter_Type;
   Compressor_Params : aliased Compress_Filter_Params;

   Bytes_Per_SE : constant := Stream_Element'Size / System.Storage_Unit;

   function Filter_Outgoing
      (Filter   : in     Compress_Filter_Type;
       F_Params : in     Filter_Params_Access;
       Stream   : access System.RPC.Params_Stream_Type)
      return Stream_Element_Array is

      dest_len   : Stream_Element_Offset;
      dest_bytes,
      src_bytes  : C.long;
      Buf        : Stream_Element_Array := To_Stream_Element_Array (Stream);

   begin
      src_bytes  := Buf'Length;
      dest_bytes := ((src_bytes + 12) * 11) / 10;
      dest_len   := Stream_Element_Offset (dest_bytes);
      declare
         Result : Stream_Element_Array (1 .. dest_len + 4);
         res    : C.int;
         len    : Integer;
      begin
         D (D_Debug, "Compressing" & src_bytes'Img & " bytes");
         if src_bytes = 0 then
            dest_bytes := 0;
         else
            res := Compress (Result (5)'Address, dest_bytes'Address,
                             Buf (Buf'First)'Address, src_bytes);
         end if;
         dest_len := Stream_Element_Offset (dest_bytes);
         D (D_Debug, "Compressed to" & dest_bytes'Img & " bytes");

         len := Buf'Length;
         D (D_Debug, "Stream length =" & dest_len'Img);

         for I in Stream_Element_Offset range 1 .. 4 loop
            Result (I) := Stream_Element (len mod 256);
            len        := len / 256;
         end loop;
         return Result (1 .. dest_len + 4);
      end;
   end Filter_Outgoing;

   function Filter_Incoming
      (Filter   : in Compress_Filter_Type;
       F_Params : in Filter_Params_Access;
       Stream   : in Ada.Streams.Stream_Element_Array)
      return Stream_Element_Array is

      Len : Stream_Element_Offset := 0;

   begin
      D (D_Debug, "Incoming length " & Stream'Length'Img);
      for I in reverse Stream_Element_Offset
                          range Stream'First .. Stream'First + 4 - 1
      loop
         Len := Len * 256 + Stream_Element_Offset (Stream (I));
      end loop;
      D (D_Debug, "Decompressing: stream length" & Len'Img);
      declare
         Result    : Stream_Element_Array (1 .. Len);
         res       : C.int;
         res_bytes : C.long := C.long (Len);
         src_bytes : C.long := Stream'Length;
      begin
         if Len > 0 then
            D (D_Debug, "Decompressing" & src_bytes'Img & " bytes");
            res := Decompress
                      (Result (1)'Address, res_bytes'Address,
                       Stream (Stream'First + 4)'Address, src_bytes - 4);
            D (D_Debug, "Decompressed length =" & res_bytes'Img);
         end if;
         return Result;
      end;
   end Filter_Incoming;

   procedure Generate_Params
      (Filter                : in  Compress_Filter_Type;
       F_Params              : out Filter_Params_Access;
       Private_F_Params      : out Filter_Params_Access;
       Needs_Params_Exchange : out Boolean) is
   begin
      F_Params              := Compressor_Params'Access;
      Private_F_Params      := Compressor_Params'Access;
      Needs_Params_Exchange := false;
   end Generate_Params;

   function Filter_Params_Read
      (Filter : Compress_Filter_Type;
       Stream : Stream_Element_Array)
     return Filter_Params_Access is
      S : aliased Params_Stream_Type (Stream'Length);
      P : Compress_Filter_Params;
   begin
      To_Params_Stream_Type (Stream, S'Access);
      Compress_Filter_Params'Read (S'Access, P);
      Print_Params (P);
      return new Compress_Filter_Params'(P);
   end Filter_Params_Read;


   function Filter_Params_Write
      (Filter : Compress_Filter_Type;
       P      : Filter_Params_Access)
     return Stream_Element_Array is
      S : aliased Params_Stream_Type (0);
   begin
      Compress_Filter_Params'Write (S'Access,
                                    Compress_Filter_Params (P.all));
      return To_Stream_Element_Array (S'Access);
   end Filter_Params_Write;

   function Get_Name (Filter : Compress_Filter_Type)
     return String is
   begin
      return "ZIP";
   end Get_Name;

   procedure Print_Params (P : Compress_Filter_Params) is
   begin
      null;
   end Print_Params;

begin
   Register_Filter (Compressor'Access);
end System.Garlic.Filters.Zip;
