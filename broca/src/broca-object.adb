with CORBA;

package body Broca.Object is

   ------------------
   -- Find_Profile --
   ------------------

   function Find_Profile (Object : Object_Ptr) return Profile_Ptr is
   begin
      return Object.Profiles (Object.Profiles'First);
   end Find_Profile;

end Broca.Object;
