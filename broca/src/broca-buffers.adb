------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        B R O C A . B U F F E R S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.13 $
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Broca.Debug;
with System; use System;

package body Broca.Buffers is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.buffers");
   procedure O is new Broca.Debug.Output (Flag);

   subtype Output_Line is String (1 .. 48);

   Hex : constant String      := "0123456789ABCDEF";
   Nil : constant Output_Line := (others => ' ');

   procedure Fix_Buffer_Size
     (Buffer : in out Buffer_Descriptor;
      Size   : in Buffer_Index_Type);

   procedure Set_Write_Mode
     (Buffer : in out Buffer_Descriptor;
      Write  : in Boolean);

   ----------------
   -- Align_Size --
   ----------------

   procedure Align_Size
     (Buffer    : in out Buffer_Descriptor;
      Alignment : in Alignment_Type) is
   begin
      Buffer.Pos := Buffer.Pos +
        ((Alignment - (Buffer.Pos mod Alignment)) mod Alignment);
   end Align_Size;

   ---------------------
   -- Allocate_Buffer --
   ---------------------

   procedure Allocate_Buffer (Buffer : in out Buffer_Descriptor) is
   begin
      Fix_Buffer_Size (Buffer, Buffer.Pos);
      Buffer.Pos := 0;
      Buffer.Little_Endian := Is_Little_Endian;
   end Allocate_Buffer;

   -----------------------------------
   -- Allocate_Buffer_And_Clear_Pos --
   -----------------------------------

   procedure Allocate_Buffer_And_Clear_Pos
     (Buffer : in out Buffer_Descriptor;
      Size   : in Buffer_Index_Type) is
   begin
      Fix_Buffer_Size (Buffer, Size);
      Buffer.Pos := 0;
   end Allocate_Buffer_And_Clear_Pos;

   -------------------
   -- Append_Buffer --
   -------------------

   procedure Append_Buffer
     (Target : in out Buffer_Descriptor;
      Source : in Buffer_Descriptor)
   is
      Length : Buffer_Index_Type
        := Source.Buffer'Last - Source.Pos + 1;
   begin
      Set_Write_Mode (Target, True);
      Target.Buffer (Target.Pos .. Target.Pos + Length - 1)
        := Source.Buffer (Source.Pos .. Source.Pos + Length - 1);
      Target.Pos := Target.Pos + Length;
   end Append_Buffer;

   ----------------------
   -- Compute_New_Size --
   ----------------------

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Align  : in Alignment_Type;
      Size   : in Buffer_Index_Type) is
   begin
      Align_Size (Buffer, Align);
      Buffer.Pos := Buffer.Pos + Size;
   end Compute_New_Size;

   ----------------------
   -- Compute_New_Size --
   ----------------------

   procedure Compute_New_Size
     (Target : in out Buffer_Descriptor;
      Source : in Buffer_Descriptor) is
   begin
      pragma Assert (Source.Buffer /= null);
      Target.Pos := Target.Pos + Source.Buffer'Last - Source.Pos + 1;
   end Compute_New_Size;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Source : in Buffer_Descriptor;
      Target : out Buffer_Descriptor) is
   begin
      Target := Source;
      if Source.Buffer /= null then
         Target.Buffer := new Buffer_Type'(Source.Buffer.all);
      end if;
   end Copy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Buffer : in out Buffer_Descriptor) is
   begin
      Free (Buffer.Buffer);
      Buffer.Pos := 0;
   end Destroy;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Buffer : Buffer_Type)
   is
      Index   : Natural := 1;
      Output  : Output_Line;
   begin
      for I in Buffer'Range loop
         Output (Index)     := ' ';
         Output (Index + 1) := Hex (Natural (Buffer (I) / 16) + 1);
         Output (Index + 2) := Hex (Natural (Buffer (I) mod 16) + 1);
         Index := Index + 3;

         if Index > Output'Length then
            pragma Debug (O (Output));
            Index := 1;
            Output := Nil;
         end if;
      end loop;

      if Index /= 1 then
         pragma Debug (O (Output (1 .. Index - 1)));
         null;
      end if;
   exception when others =>
      pragma Debug (O ("problem in dump"));
      raise;
   end Dump;

   --------------------
   -- Extract_Buffer --
   --------------------

   procedure Extract_Buffer
     (Target : in out Buffer_Descriptor;
      Source : in out Buffer_Descriptor;
      Length : in Buffer_Index_Type) is
   begin
      Set_Write_Mode (Target, False);
      Target.Buffer (Target.Pos .. Target.Pos + Length - 1) :=
        Source.Buffer (Source.Pos .. Source.Pos + Length - 1);
      Source.Pos := Source.Pos + Length;
      Target.Pos := 0;
      Target.Little_Endian := Source.Little_Endian;
   end Extract_Buffer;

   -----------------------
   -- Fix_Buffer_Size --
   -----------------------

   procedure Fix_Buffer_Size
     (Buffer : in out Buffer_Descriptor;
      Size   : in Buffer_Index_Type) is
   begin
      if Buffer.Buffer /= null then
         if Size = Buffer_Index_Type (Buffer.Buffer'Length) then
            return;
         end if;
         Free (Buffer.Buffer);
      end if;

      if Size > 0 then
         Buffer.Buffer := new Buffer_Type (0 .. Size - 1);
      end if;
   end Fix_Buffer_Size;

   ---------------
   -- Full_Size --
   ---------------

   function Full_Size (Buffer : in Buffer_Descriptor)
     return Buffer_Index_Type is
   begin
      if Buffer.Buffer /= null then
         return Buffer.Buffer'Last + 1;
      else
         return Buffer.Pos;
      end if;
   end Full_Size;

   -------------------
   -- Get_Endianess --
   -------------------

   function Get_Endianess (Buffer : Buffer_Descriptor) return Boolean is
   begin
      return Buffer.Little_Endian;
   end Get_Endianess;

   ----------
   -- Read --
   ----------

   procedure Read
     (Buffer  : in out Buffer_Descriptor;
      Bytes   : out Buffer_Type)
   is
      Length : constant Buffer_Index_Type := Buffer_Index_Type (Bytes'Length);
   begin
      Set_Write_Mode (Buffer, False);

      if Length = 0 then
         return;
      end if;

      pragma Assert (Buffer.Pos + Length - 1 <= Buffer.Buffer'Last);
      Bytes := Buffer.Buffer (Buffer.Pos .. Buffer.Pos + Length - 1);
      Buffer.Pos := Buffer.Pos + Length;
   end Read;

   ------------
   -- Rewind --
   ------------

   procedure Rewind (Buffer : in out Buffer_Descriptor) is
   begin
      Buffer.Pos := 0;
   end Rewind;

   -------------------
   -- Set_Endianess --
   -------------------

   procedure Set_Endianess
     (Buffer        : in out Buffer_Descriptor;
      Little_Endian : in Boolean) is
   begin
      Buffer.Little_Endian := Little_Endian;
   end Set_Endianess;

   --------------------
   -- Set_Write_Mode --
   --------------------

   procedure Set_Write_Mode
     (Buffer : in out Buffer_Descriptor;
      Write  : in Boolean)
   is
   begin
      if Buffer.Write and not Write then
         --  If reading after writing, then
         --  rewind buffer.
         Buffer.Pos := 0;
      end if;
      Buffer.Write := Write;
   end Set_Write_Mode;

   ---------------
   -- Size_Left --
   ---------------

   function Size_Left
     (Buffer : in Buffer_Descriptor)
      return Buffer_Index_Type is
   begin
      return Buffer.Buffer'Last - Buffer.Pos + 1;
   end Size_Left;

   ---------------
   -- Size_Used --
   ---------------

   function Size_Used
     (Buffer : in Buffer_Descriptor)
      return Buffer_Index_Type is
   begin
      return Buffer.Pos;
   end Size_Used;

   ----------
   -- Show --
   ----------

   procedure Show (Buffer : in Buffer_Descriptor) is
   begin
      pragma Debug (O ("Pos  =" & Buffer.Pos'Img));
      if Buffer.Buffer /= null then
         pragma Debug (O ("Last =" & Buffer.Buffer'Last'Img));
         Dump (Buffer.Buffer (Buffer.Pos .. Buffer.Buffer'Last));
      else
         pragma Debug (O ("Buffer empty"));
         null;
      end if;
   end Show;

   ----------------
   -- Skip_Bytes --
   ----------------

   procedure Skip_Bytes
     (Buffer : in out Buffer_Descriptor;
      Size   : in Buffer_Index_Type) is
   begin
      Buffer.Pos := Buffer.Pos + Size;
   end Skip_Bytes;

   -----------
   -- Write --
   -----------

   procedure Write
     (Buffer  : in out Buffer_Descriptor;
      Bytes   : in Buffer_Type)
   is
      Length : constant Buffer_Index_Type := Buffer_Index_Type (Bytes'Length);
   begin
      Set_Write_Mode (Buffer, True);
      pragma Assert (Buffer.Pos + Length - 1 <= Buffer.Buffer'Last);
      Buffer.Buffer (Buffer.Pos .. Buffer.Pos + Length - 1) := Bytes;
      Buffer.Pos := Buffer.Pos + Length;
   end Write;

begin
   Is_Little_Endian := (Default_Bit_Order /= High_Order_First);
end Broca.Buffers;

