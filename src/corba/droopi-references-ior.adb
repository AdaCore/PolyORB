------------------------------------------------------------------------------
--                                                                          --
--                     DROOPI COMPONENTS
--                       IOR BODY                                           --

------------------------------------------------------------------------------

with CORBA;
with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Log;

package body Droopi.References.Ior is


   package L is new Droopi.Log.Facility_Log ("droopi.protocols.giop");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Ior_Type) is
   begin
        
      Marshall (Buffer, Value.Type_Id);
      Marshall (Buffer, CORBA.Unsigned_Long (Value.Profiles'Length));

      for N in Profiles'Range loop
         Marshall (Buffer, Get_Profile_Tag (Value.Profiles (N).all));
         Callbacks (Get_Profile_Tag (Value.Profiles (N).all)).Marshall_Profile_Body
           (Buffer, Value.Profiles (N));
      end loop;

   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Result : out Ior_Type)
   is
       N_Profiles   : CORBA.Unsigned_Long;
       Prof_Array   :  access Profile_Array;
   begin

      Result.Type_Id := Unmarshall (Buffer);
      N_Profiles := Unmarshall (Buffer);
      pragma Debug (O ("Decapsulate_IOR: type "
                       & To_Standard_String (Type_Id)
                       & " (" & N_Profiles'Img & " profiles)."));

      Prof_Array  := new Profile_Array(1 .. N_Profiles => null);

      for N in Profiles'Range loop
         declare
            Profile    : constant Profile_Tag
              := Unmarshall (Buffer);
         begin
            Callbacks (Profile).Unmarshall_Profile_Body
              (Buffer, Profiles (N));
         end;
      end loop;

      Result.Profiles := To_Sequence (Prof_Array); 

   end Unmarshall;

  

end Droopi.References.Ior;
