with Ada.Tags;
with CORBA;

package body Broca.Object is
   --  Find a profile for a message.
   function Find_Profile (Object : Object_Acc) return Profile_Acc is
   begin
      return Object.Profiles (Object.Profiles'First);
   end Find_Profile;
end Broca.Object;
