with Ada.Text_IO; use Ada.Text_IO;
with Broca.CDR;   use Broca.CDR;
with CORBA;       use CORBA;

procedure Test_CDR is

   Stream : aliased Buffer_Type;

   procedure Dump (S : access Buffer_Type);

   ----------
   -- Dump --
   ----------

   procedure Dump (S : access Buffer_Type) is

      function Hex (O : Octet) return String;

      ---------
      -- Hex --
      ---------

      function Hex (O : Octet) return String is
         Mapping : constant String := "0123456789ABCDEF";
      begin
         return Mapping ((Natural (O) / 16) + 1) &
           Mapping ((Natural (O) mod 16) + 1);
      end Hex;

      Content : constant Octet_Array := Get_Content (S);
      Count   : Natural              := 0;

   begin
      Put ("Byte ordering: ");
      if Content (Content'First) = 0 then
         Put_Line ("big endian");
      elsif Content (Content'First) = 1 then
         Put_Line ("little endian");
      else
         Put_Line ("invalid");
      end if;
      for I in Content'Range loop
         Put (Hex (Content (I)) & " ");
         Count := Count + 1;
         if Count = 25 then
            New_Line;
            Count := 0;
         end if;
      end loop;
      New_Line;
   end Dump;

begin
   Marshall (Stream'Access, True);
   Marshall (Stream'Access, CORBA.Unsigned_Long'(16#1234ABCD#));
   Dump (Stream'Access);
end Test_CDR;
