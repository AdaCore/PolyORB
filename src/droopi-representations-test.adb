--  A dummy data representation method, just for show.

--  $Id$

package body Droopi.Representations.Test is

   procedure Marshall_From_Any
     (R      : Rep_Test;
      Buffer : access Buffers.Buffer_Type;
      Data   : CORBA.Any) is
   begin
      raise Not_Implemented;
   end Marshall_From_Any;

   procedure Unmarshall_To_Any
     (R      : Rep_Test;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out CORBA.Any) is
   begin
      raise Not_Implemented;
   end Unmarshall_To_Any;

   procedure Marshall_Char
     (B : access Buffer_Type;
      C : Character) is
   begin
      Align_Marshall_Copy
        (B, Stream_Element_Array'
         (1 => Stream_Element (Character'Pos (C))));
   end Marshall_Char;

   function Unmarshall_Char
     (B : access Buffer_Type)
     return Character
   is
      A : constant Stream_Element_Array
        := Align_Unmarshall_Copy (B, 1);
   begin
      return Character'Val (A (A'First));
   end Unmarshall_Char;

   procedure Marshall_String
     (R : access Rep_Test;
      B : access Buffer_Type;
      S : String)
   is
   begin
      for I in S'Range loop
         Marshall_Char (B, S (I));
      end loop;
   end Marshall_String;

   function Unmarshall_String
     (R : access Rep_Test;
      B : access Buffer_Type)
     return String
   is
      S : String (1 .. 1024);
      C : Character;
      Last : Integer := S'First - 1;
      Max : constant Stream_Element_Count
        := Length (B);
   begin
      loop
         exit when Last - S'First + 1 = Integer (Max);
         C := Unmarshall_Char (B);
         if C = ASCII.CR then
            C := Unmarshall_Char (B);
            pragma Assert (C = ASCII.LF);

            exit;
         end if;
         Last := Last + 1;
         S (Last) := C;
         exit when Last = S'Last;
      end loop;

      return S (S'First .. Last);
   end Unmarshall_String;

end Droopi.Representations.Test;
