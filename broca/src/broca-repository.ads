with CORBA;
with CORBA.Object;

package Broca.Repository is
   --  The repository contains all classes known (ie, which stubs exist for).
   type Object_Class_Type;
   type Object_Class_Ptr is access Object_Class_Type'Class;
   type Object_Class_Type is abstract tagged
      record
         Next : Object_Class_Ptr;
         Type_Id : CORBA.RepositoryId;
      end record;

   --  primitive operation for object_class_type:
   --  Create a new object of this class.
   function Create_Object (Class : access Object_Class_Type)
                           return CORBA.Object.Ref'Class is abstract;

   --  Add a new class to the repository.
   procedure Register (Class : Object_Class_Ptr);

   --  Create an object from a type_id
   function Create_Ref (Type_Id : CORBA.RepositoryId)
                        return CORBA.Object.Ref'Class;
end Broca.Repository;
