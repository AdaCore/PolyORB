with CORBA;
with CORBA.Object;

package Broca.Repository is

   --  The repository contains all the known factories.

   type Factory_Type;
   type Factory_Ptr is access Factory_Type'Class;
   type Factory_Type is abstract tagged
      record
         Next    : Factory_Ptr;
         Type_Id : CORBA.RepositoryId;
      end record;

   --  Primitive operation for a factory: create a new object.
   function Create
     (Factory : access Factory_Type)
     return CORBA.Object.Ref'Class is abstract;

   --  Add a new factory to the repository.
   procedure Register (Factory : in Factory_Ptr);

   --  Create an object from a type_id
   function Create
     (Type_Id : CORBA.RepositoryId)
     return CORBA.Object.Ref'Class;

end Broca.Repository;
