------------------------------------------------------------------------------
--                     DROOPI COMPONENTS
--                       IOR BODY                                           --
------------------------------------------------------------------------------



with Ada.Streams; use Ada.Streams;

with CORBA;
with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Log;
with Droopi.Opaque;
with Droopi.Representations.CDR; use Droopi.Representations.CDR;
with Droopi.CORBA_P.Exceptions; use Droopi.CORBA_P.Exceptions;

pragma Elaborate_All (Droopi.Log);



package body Droopi.References.IOR is

   use Droopi.Log;

   package L is
      new Droopi.Log.Facility_Log ("droopi.references.ior");

   procedure O
     (Message : in String;
      Level   : in Log_Level := Debug)
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
      use Profile_Seqs;
      Profs  : Profile_Array := Profiles_Of (Value.Ref);
   begin
      Marshall (Buffer, Value.Type_Id);
      Marshall (Buffer, CORBA.Unsigned_Long (Profs'Length));

      for N in Profs'Range loop
         Marshall (Buffer, CORBA.Unsigned_Long (Get_Profile_Tag
                                                (Profs (N).all)));
         Callbacks
           (Get_Profile_Tag (Profs (N).all)).
            Marshall_Profile_Body (Buffer, Profs (N));
      end loop;
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer : access Buffer_Type)
   return  IOR_Type
   is
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
                     Tag      : constant Profile_Tag := Profile_Tag (Temp_Tag);
               begin
                     Profs (N) := Callbacks (Tag).
                               Unmarshall_Profile_Body (Buffer);
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
      Marshall (Buf, IOR);
      declare
            Octets    : Encapsulation := Encapsulate (Buf);
            Str         : String (1 ..  4 + Natural (Octets'Length) * 2);
      begin
         Str (1 .. 4) := "IOR:";
         for I in Octets'Range loop
            Str (5 + 2 * Natural (I - Octets'First))
                := Hexa_Digits (Integer (Octets (I)) / 16);
            Str (6 + 2 * Natural (I - Octets'First))
                := Hexa_Digits (Integer (Octets (I)) mod 16);
         end loop;
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
      use Droopi.Opaque;
      Buf     : Buffer_Access := new Buffer_Type;
      IOR     : IOR_Type;
      S       : String := To_Standard_String (Str);
      Length  : Natural := S'Length;
      Octets  : Zone_Access := new Encapsulation (1 ..
                      Stream_Element_Offset ((Length - 4) / 2));
   begin

      if Length <= 4
        or else Length mod 2 /= 0
        or else S (S'First .. S'First + 3) /= "IOR:" then
         CORBA_P.Exceptions.Raise_Bad_Param;
      end if;

      declare
         Oct   : Stream_Element;
         Index : Stream_Element_Offset := Octets'First;
      begin

         for I in S'First + 4 .. S'Last loop
            if S (I) >= '0' and then S (I) <= '9' then
               Oct := Character'Pos (S (I)) - Character'Pos ('0');
            elsif S (I) >= 'A' and then S (I) <= 'F' then
               Oct := 10 + Character'Pos (S (I)) - Character'Pos ('A');
            elsif S (I) >= 'a' and then S (I) <= 'f' then
               Oct := 10 + Character'Pos (S (I)) - Character'Pos ('a');
            else
               CORBA_P.Exceptions.Raise_Bad_Param;
            end if;
            if I mod 2 = 1 then
               Octets (Index) := Oct * 16;
            else
               Octets (Index) := Octets (Index) + Oct;
               Index := Index + 1;
            end if;
         end loop;

         Decapsulate (Octets, Buf);
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
   begin
      Callbacks (Profile).Marshall_Profile_Body := Marshall_Profile_Body;
      Callbacks (Profile).Unmarshall_Profile_Body := Unmarshall_Profile_Body;
   end Register;

end Droopi.References.IOR;
