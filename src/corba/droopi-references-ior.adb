------------------------------------------------------------------------------
--                     DROOPI COMPONENTS
--                       IOR BODY                                           --
------------------------------------------------------------------------------



with Ada.Streams; use Ada.Streams;

with CORBA;
with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

with Droopi.Utils;
with Droopi.Representations.CDR;
with Droopi.CORBA_P.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Sequences.Unbounded;
with Droopi.Binding_Data;
with Droopi.Types;


package body Droopi.References.IOR is

   use Droopi.Log;
   use Droopi.Utils;
   use Droopi.Representations.CDR;
   use Droopi.CORBA_P.Exceptions;
   use Droopi.Binding_Data;
   use Profile_Record_Seq;

   package L is new Droopi.Log.Facility_Log ("droopi.references.ior");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Hexa_Digits : constant array (0 .. 15) of Character
     := "0123456789abcdef";


   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in IOR_Type)
   is
      use CORBA;
      use Droopi.Types;
      use Profile_Seqs;
      Profs  : Profile_Array := Profiles_Of (Value.Ref);

   begin



      Marshall (Buffer, Value.Type_Id);
      Marshall (Buffer, CORBA.Unsigned_Long (Profs'Length));
      --  Put_Line ("Length" & (Integer'Image (Integer (Profs'Length))));

      pragma Debug (O ("Marshall Profile : Enter"));

      for N in Profs'Range loop
         Marshall (Buffer, CORBA.Unsigned_Long (Get_Profile_Tag
                                                (Profs (N).all)));
         pragma Debug
              (O (Integer'Image (Integer (CORBA.Unsigned_Long
                    (Get_Profile_Tag (Profs (N).all)))) & ASCII.LF));
         Put_Line ("Profile_tag : " & (Integer'Image (Integer
                    (CORBA.Unsigned_Long
                    (Get_Profile_Tag (Profs (N).all))))));

         for I in 1 .. Length (Callbacks) loop
            if Element_Of (Callbacks, I).Tag =
               Get_Profile_Tag (Profs (N).all) then
               Element_Of (Callbacks, I).
               Marshall_Profile_Body (Buffer, Profs (N));
            end if;
         end loop;
      end loop;

      pragma Debug (O ("Marshall Profile : Leave"));

   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer : access Buffer_Type)
   return  IOR_Type
   is
      use Droopi.Types;
      use CORBA;
      use Profile_Seqs;
      N_Profiles : CORBA.Unsigned_Long;
      Result     : IOR_Type;
   begin

      Result.Type_Id := Unmarshall (Buffer);
      N_Profiles     := Unmarshall (Buffer);

      declare
            Profs      : Profile_Array := (1 .. Integer (N_Profiles) => null);
      begin

            pragma Debug
                (O ("Decapsulate_IOR: type " &
                 To_Standard_String (Result.Type_Id) &
                 " (" & N_Profiles'Img & " profiles)."));

            for N in Profs'Range loop
               declare
                  Temp_Tag : CORBA.Unsigned_Long := Unmarshall (Buffer);
                  Tag      : constant Profile_Tag :=
                                 Profile_Tag (Temp_Tag);
               begin
                  for I in 1 .. Length (Callbacks) loop
                     if Element_Of (Callbacks, I).Tag = Tag then
                        Profs (N) := Element_Of (Callbacks, I).
                                     Unmarshall_Profile_Body (Buffer);
                     end if;
                  end loop;
                  --       Profs (N) := Callbacks (Tag).
                  --          Unmarshall_Profile_Body (Buffer);

               end;
            end loop;

            Result.Ref.Profiles := To_Sequence (Profs);
            return Result;
      end;
   end Unmarshall;


   -------------------------------------------------------
   --  Stringfiecation of an IOR: from an Object to String
   -------------------------------------------------------

   function Object_To_String
     (IOR : IOR_Type)
     return CORBA.String
   is
      use CORBA;
      Buf : Buffer_Access := new Buffer_Type;

   begin

      pragma Debug (O ("Object to string : Enter"));

      Marshall (Buf, IOR);

      declare
            Octets : Encapsulation := Encapsulate (Buf);
            S      : String :=  To_String (Stream_Element_Array (Octets));
            Str    : String := "IOR:" & S;
      begin
         pragma Debug (O ("Object to string : Leave"));
         return To_CORBA_String (Str);
      end;



   end Object_To_String;



   -----------------------------------------------------------
   --  Destringfiecation of an IOR: from an String to an Object
   -----------------------------------------------------------

   function String_To_Object
     (Str : CORBA.String)
      return IOR_Type
   is
      use CORBA;
      use Droopi.Buffers;
      Buf     : Buffer_Access := new Buffer_Type;
      IOR     : IOR_Type;
      S       : String := To_Standard_String (Str);
      Length  : Natural := S'Length;

   begin

      if Length <= 4
        or else Length mod 2 /= 0
        or else S (S'First .. S'First + 3) /= "IOR:" then
         CORBA_P.Exceptions.Raise_Bad_Param;
      end if;

      declare
            Octets : aliased Encapsulation  :=
                      Encapsulation (To_Stream_Element_Array (S
                                    (S'First + 4 .. S'Last)));
            --  Octets : aliased Encapsulation  :=
            --         Encapsulation (To_Stream_Element_Array (
            --                        ("allons-y")));
      begin
            Set_Initial_Position (Buf, 0);
            Put_Line ("CDR : " &
              Stream_Element_Offset'Image (CDR_Position (Buf)));
            Decapsulate (Octets'Access, Buf);
            Set_Initial_Position (Buf, 0);
            Put_Line ("CDR : " &
              Stream_Element_Offset'Image (CDR_Position (Buf)));
            Show (Buf.all);

            --   declare
            --    Tt : CORBA.Unsigned_Long := Unmarshall (Buf);
            --   begin
            --    Put_Line ("LONG : " &
            --       CORBA.Unsigned_Long'Image (Tt));
            --   Put_Line ("CDR : " &
            --   Stream_Element_Offset'Image (CDR_Position (Buf)));
            --  end;

            --  return IOR;

            IOR := Unmarshall (Buf);
            return IOR;
      end;

   end String_To_Object;


   --------------
   -- Register --
   --------------


   procedure Register
     (Profile     : in Profile_Tag;
      Marshall_Profile_Body   : in Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : in Unmarshall_Profile_Body_Type) is

      Elt : constant Profile_Record := (Profile, Marshall_Profile_Body,
                                        Unmarshall_Profile_Body);
   begin
      Append (Callbacks, Elt);
   end Register;


end Droopi.References.IOR;
