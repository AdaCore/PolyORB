------------------------------------------------------------------------------
--                     DROOPI COMPONENTS
--                       IOR BODY                                           --
------------------------------------------------------------------------------

with CORBA;
with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Log;

package body Droopi.References.IOR is

   package L is new Droopi.Log.Facility_Log ("droopi.protocols.giop");
   procedure O
     (Message : in String;
      Level   : in Log_Level := Debug)
     renames L.Output;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in IOR_Type) is
   begin
      Marshall (Buffer, Value.Type_Id);
      Marshall (Buffer, CORBA.Unsigned_Long (Value.Profiles'Length));

      for N in Value.Profiles'Range loop
         Marshall (Buffer, Get_Profile_Tag (Value.Profiles (N).all));
         Callbacks
           (Get_Profile_Tag
            (Value.Profiles (N).all)).Marshall_Profile_Body
             (Buffer, Value.Profiles (N));
      end loop;
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : access Buffer_Type)
   return  IOR_Type;

   is
       N_Profiles : CORBA.Unsigned_Long;
       Profiles   : access Profile_Array;
       Result : out IOR_Type;
   begin
      Result.Type_Id := Unmarshall (Buffer);
      N_Profiles     := Unmarshall (Buffer);

      pragma Debug
        (O ("Decapsulate_IOR: type " &
            To_Standard_String (Type_Id) &
            " (" & N_Profiles'Img & " profiles)."));

      Prof_Array := new Profile_Array (1 .. N_Profiles => null);

      for N in Profiles'Range loop
         declare
            Tag : constant Profile_Tag := Unmarshall (Buffer);
         begin
            Callbacks (Tag).Unmarshall_Profile_Body (Buffer, Profiles (N));
         end;
      end loop;

      Result.Profiles := To_Sequence (Prof_Array);
      return Result;
   end Unmarshall;

end Droopi.References.IOR;
