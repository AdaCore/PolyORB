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

   ---------------------------------
   -- Allocate_Buffer_And_Set_Pos --
   ---------------------------------

   procedure Allocate_Buffer_And_Set_Pos
     (Buffer : in out Buffer_Descriptor;
      Size   : in Buffer_Index_Type) is
   begin
      Fix_Buffer_Size (Buffer, Size);
      Buffer.Pos := Size;
   end Allocate_Buffer_And_Set_Pos;

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
      Source : in Buffer_Descriptor) is
   begin
      Target.Buffer (Target.Pos .. Target.Pos + Source.Pos - 1)
        := Source.Buffer (0 .. Source.Pos - 1);
      Target.Pos := Target.Pos + Source.Pos;
   end Append_Buffer;

   ------------------
   -- Compute_Size --
   ------------------

   procedure Compute_Size
     (Target : in out Buffer_Descriptor;
      Source : in Buffer_Descriptor) is
   begin
      Target.Pos := Target.Pos + Source.Pos;
   end Compute_Size;

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

   procedure Extract_Buffer (Target : in out Buffer_Descriptor;
                             Source : in out Buffer_Descriptor;
                             Length : Buffer_Index_Type) is
   begin
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
      if Buffer.Buffer = null or else Buffer.Buffer.all'Last < Size - 1 then
         Free (Buffer.Buffer);
         Buffer.Buffer := new Buffer_Type (0 .. Size - 1);
      end if;
   end Fix_Buffer_Size;

begin
   Is_Little_Endian := (Default_Bit_Order /= High_Order_First);
end Broca.Buffers;

