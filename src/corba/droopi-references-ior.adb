------------------------------------------------------------------------------
--                     DROOPI COMPONENTS
--                       IOR BODY                                           --
------------------------------------------------------------------------------

with CORBA;
with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Log;
with Droopi.Representations.CDR; use Droopi.Representations.CDR;
with Droopi.Binding_Data;


package body Droopi.References.IOR is

   use Droopi.Log;

   package L is
      new Droopi.Log.Facility_Log ("droopi.references.ior");

   procedure O
     (Message : in String;
      Level   : in Log_Level := Debug)
     renames L.Output;

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
                     Profs (N) :=
                       Callbacks (Tag).Unmarshall_Profile_Body (Buffer);
               end;
            end loop;

            Result.Ref.Profiles := To_Sequence (Profs);
            return Result;
      end;
   end Unmarshall;


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
