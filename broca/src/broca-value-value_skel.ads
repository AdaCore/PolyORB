with Broca.Value.Operation_Store;
with Corba;

package Broca.Value.Value_Skel is

   pragma Elaborate_Body;

   type Is_A_Type is access
     function
     (Type_Id : in CORBA.RepositoryId)
     return CORBA.Boolean;

   --  This is where we store all the Is_A operations for
   --  valuetypes
   package Is_A_Store is new Broca.Value.Operation_Store
     (Is_A_Type);

end Broca.Value.Value_Skel;
