with CORBA; use CORBA;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Repository is
   Flag : constant Natural := Broca.Debug.Is_Active ("broca.repository");
   procedure O is new Broca.Debug.Output (Flag);

   --  Single linked list of all classes.
   Classes : Object_Class_Acc;

   --  Add a new class to the repository.
   procedure Register (Class : Object_Class_Acc) is
   begin
      pragma Debug (O ("Register : enter"));
      pragma Debug (O ("Create_Ref : Repository_Id = " &
                       CORBA.To_Standard_String(Corba.String(Class.all.Type_Id))));
      --  Simply add it to the list.
      Class.Next := Classes;
      Classes := Class;
   end Register;

   --  Create an object from a type_id
   function Create_Ref (Type_Id : CORBA.RepositoryId)
                        return CORBA.Object.Ref'Class
   is
      El : Object_Class_Acc;
      Res : CORBA.Object.Ref;
   begin
      pragma Debug (O ("Create_Ref : enter"));
      pragma Debug (O ("Create_Ref : Repository_Id = " &
                       CORBA.To_Standard_String(Corba.String(Type_Id))));
      El := Classes;
      while El /= null loop
         if El.Type_Id = Type_Id then
            return Create_Object (El);
         end if;
         El := El.Next;
      end loop;
      --  Return a null object.
      pragma Debug (O ("Create_ref : Object type not found"));
      CORBA.Object.Set (Res, null);
      return Res;
   end Create_Ref;
end Broca.Repository;



