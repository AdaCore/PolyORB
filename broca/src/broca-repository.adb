with CORBA; use CORBA;
with Ada.Tags;

package body Broca.Repository is
   --  Single linked list of all classes.
   Classes : Object_Class_Acc;

   --  Add a new class to the repository.
   procedure Register (Class : Object_Class_Acc) is
   begin
      --  Simply add it to the list.
      Class.Next := Classes;
      Classes := Class;
   end Register;

--    function Create_Object (Class: access Object_Class_Type)
--                            return CORBA.Object.Ref'Class is
--    begin
--       Ada.Text_Io.Put_Line ("bad create_object called");
--       raise Program_Error;
--       return CORBA.Object.Ref'(Object => null);
--    end Create_Object;

   --  Create an object from a type_id
   function Create_Ref (Type_Id : CORBA.RepositoryId)
                        return CORBA.Object.Ref'Class
   is
      El : Object_Class_Acc;
      Res : CORBA.Object.Ref;
   begin
      El := Classes;
      while El /= null loop
         if El.Type_Id = Type_Id then
            return Create_Object (El);
         end if;
         El := El.Next;
      end loop;
      --  Return a null object.
      CORBA.Object.Set (Res, null);
      return Res;
   end Create_Ref;
end Broca.Repository;



