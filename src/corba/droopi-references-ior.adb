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
      use Droopi.Types;
      use Profile_Seqs;
      Profs  : Profile_Array := Profiles_Of (Value.Ref);
      Counter  : Integer := 0;

   begin
      Marshall (Buffer, Types.String (Value.Type_Id));
      Marshall (Buffer, Types.Unsigned_Long (Length (Callbacks)));

      pragma Debug (O ("Marshall Profile : Enter"));

      for N in Profs'Range loop
         for I in 1 .. Length (Callbacks) loop
            if Element_Of (Callbacks, I).Tag =
               Get_Profile_Tag (Profs (N).all) then
               Marshall (Buffer, Types.Unsigned_Long (Get_Profile_Tag
                         (Profs (N).all)));

               Element_Of (Callbacks, I).
                         Marshall_Profile_Body (Buffer, Profs (N));
               Counter := Counter + 1;
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

      N_Profiles : Types.Unsigned_Long;
      Result     : IOR_Type;
   begin

      Result.Type_Id := CORBA.String (Types.String'(Unmarshall (Buffer)));
      N_Profiles     := Unmarshall (Buffer);

      declare
            Profs      : Profile_Array := (1 .. Integer (N_Profiles) => null);
      begin

            pragma Debug
                (O ("Decapsulate_IOR: type " &
                    CORBA.To_Standard_String (Result.Type_Id) &
                    " (" & N_Profiles'Img & " profiles)."));

            for N in Profs'Range loop
               declare
                  Temp_Tag : Types.Unsigned_Long := Unmarshall (Buffer);
                  Tag      : constant Profile_Tag :=
                                 Profile_Tag (Temp_Tag);
               begin

                  for I in 1 .. Length (Callbacks) loop
                     if Element_Of (Callbacks, I).Tag = Tag then
                        Profs (N) := Element_Of (Callbacks, I).
                          Unmarshall_Profile_Body (Buffer);
                        --  Profiles dynamically allocated here
                        --  will be freed when the returned
                        --  reference is finalised.
                     end if;
                  end loop;
               end;
            end loop;

            Create_Reference (Profs, Result.Ref);

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

      Start_Encapsulation (Buf);
      Marshall (Buf, IOR);

      declare
            Octets : constant Encapsulation := Encapsulate (Buf);
            S      : constant String :=  "IOR:"
              & To_String (Stream_Element_Array (Octets));
      begin
         pragma Debug (O ("Object to string : Leave"));
         Release (Buf);
         return CORBA.To_CORBA_String (S);
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
      S       : constant String
        := CORBA.To_Standard_String (Str);
      Length  : constant Natural := S'Length;

   begin

      if Length <= 4
        or else Length mod 2 /= 0
        or else S (S'First .. S'First + 3) /= "IOR:" then
         CORBA_P.Exceptions.Raise_Bad_Param;
      end if;

      declare
         Octets : aliased Encapsulation
           := Encapsulation
           (To_Stream_Element_Array (S (S'First + 4 .. S'Last)));
      begin
         Decapsulate (Octets'Access, Buf);
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
