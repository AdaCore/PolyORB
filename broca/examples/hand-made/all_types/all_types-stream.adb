with Broca.CDR;
--  with Broca.Refs;
--  with Broca.Exceptions;
with all_types.Stream;
use Broca.CDR;
--  use Broca.Refs;
--  use Broca.Exceptions;
use all_types.Stream;
package body all_types.Stream is

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : Color)
   is
   begin
      Marshall
        (Stream,
         CORBA.Unsigned_Long (Color'Pos (Val)));
   end Marshall;

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
     return Color
   is
   begin
      return Color'Val (CORBA.Unsigned_Long'(Unmarshall
                                             (Stream)));
   end Unmarshall;

   procedure Marshall
     (Stream : access Broca.Buffers.Buffer_Type;
      Val : in my_exception_Members)
   is
      use Broca.CDR;
   begin
      Marshall (Stream, Val.info);
   end Marshall;

   function Unmarshall
     (Stream : access Broca.Buffers.Buffer_Type)
      return my_exception_Members
   is
      use Broca.CDR;
      Res : my_exception_Members;
   begin
      Res.info := Unmarshall (Stream);
      return Res;
   end Unmarshall;

--     procedure Unmarshall_And_Raise_my_exception
--        (Stream : access Broca.Buffers.Buffer_Type)
--     is
--        Bod : Broca.Exceptions.IDL_Exception_Members_Ptr;
--        Dummy : CORBA.String;
--     begin
--        Bod := new my_exception_Members;
--        --  XXX thomas 2000-02-28 ??
--        --  Skip_String (Stream);
--        Dummy := Unmarshall (Stream);
--        Bod.all := Unmarshall (Stream);
--        Broca.Exceptions.User_Raise_Exception
--           (my_exception'Identity, Bod);
--     end Unmarshall_And_Raise_my_exception;
--
--     procedure Raise_my_exception
--        (Bod : my_exception_Members)
--     is
--        Members : Broca.Exceptions.IDL_Exception_Members_Ptr;
--     begin
--        Members := new my_exception_Members'(Bod);
--        Broca.Exceptions.User_Raise_Exception
--           (my_exception'Identity, Members);
--     end Raise_my_exception;

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : myUnion)
   is
   begin
      Marshall (Stream, Val.Switch);
      case Val.Switch is
         when 1 => Marshall (Stream, Val.Counter);
         when 2 => Marshall (Stream, Val.Flag);
         when 3 => Marshall (Stream, Val.Hue);
         when others => Marshall (Stream, Val.Unknown);
      end case;
   end Marshall;

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return myUnion
   is
      Switch : CORBA.Long;
   begin
      Switch := Unmarshall (Stream);
      declare
         Tmp : myUnion (Switch);
      begin
         case Switch is
            when 1 => Tmp.Counter := Unmarshall (Stream);
            when 2 => Tmp.Flag := Unmarshall (Stream);
            when 3 => Tmp.Hue := Unmarshall (Stream);
            when others => Tmp.Unknown := Unmarshall (Stream);
         end case;
         return Tmp;
      end;
   end Unmarshall;

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : simple_array)
   is
   begin
      for I1 in Val'Range (1) loop
         Marshall (Stream, Val (I1));
      end loop;
   end Marshall;

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return simple_array
   is
      Res : simple_array;
   begin
      for I1 in Res'Range (1) loop
         Res (I1) := Unmarshall (Stream);
      end loop;
      return Res;
   end Unmarshall;

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : matrix)
   is
   begin
      for I1 in Val'Range (1) loop
         for I2 in Val'Range (2) loop
            Marshall (Stream, Val (I1, I2));
         end loop;
      end loop;
   end Marshall;

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return matrix
   is
      Res : matrix;
   begin
      for I1 in Res'Range (1) loop
         for I2 in Res'Range (2) loop
            Res (I1, I2) := Unmarshall (Stream);
         end loop;
      end loop;
      return Res;
   end Unmarshall;

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : a_Array)
   is
   begin
      for I1 in Val'Range (1) loop
         Marshall (Stream, Val (I1));
      end loop;
   end Marshall;

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return a_Array
   is
      Res : a_Array;
   begin
      for I1 in Res'Range (1) loop
         Res (I1) := Unmarshall (Stream);
      end loop;
      return Res;
   end Unmarshall;

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : simple_struct)
   is
   begin
      Marshall (Stream, Val.a);
      Marshall (Stream, Val.s);
   end Marshall;

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return simple_struct
   is
      Res : simple_struct;
   begin
      Res.a := Unmarshall (Stream);
      Res.s := Unmarshall (Stream);
      return Res;
   end Unmarshall;

   procedure Marshall
     (Stream : access Broca.Buffers.Buffer_Type;
      Val : in IDL_Sequence_Short.Sequence)
   is
      use IDL_Sequence_Short;

      Elements : Element_Array
        := To_Element_Array (Val);
      Len : CORBA.Unsigned_Long
        := CORBA.Unsigned_Long (Elements'Length);
   begin
      Marshall (Stream, Len);
      for I in Elements'Range loop
         Marshall (Stream, Elements (I));
      end loop;
   end Marshall;

   function Unmarshall
     (Stream : access Broca.Buffers.Buffer_Type)
      return IDL_Sequence_Short.Sequence
   is
      use IDL_Sequence_Short;

      Len : CORBA.Unsigned_Long;
   begin
      Len := Unmarshall (Stream);
      declare
         Elements : Element_Array (1 .. Integer (Len));
      begin
         for I in Elements'Range loop
            Elements (I) := Unmarshall (Stream);
         end loop;
         return IDL_Sequence_Short.To_Sequence (Elements);
      end;
   end Unmarshall;

   procedure Marshall
      (Stream : access Broca.Buffers.Buffer_Type;
       Val : U_sequence)
   is
   begin
      Marshall (Stream, IDL_Sequence_Short.Sequence (Val));
   end Marshall;

   function Unmarshall
      (Stream : access Broca.Buffers.Buffer_Type)
       return U_sequence
   is
   begin
      return U_sequence
        (IDL_Sequence_Short.Sequence'(Unmarshall (Stream)));
   end Unmarshall;

end all_types.Stream;
