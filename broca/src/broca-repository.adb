with CORBA; use CORBA;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Repository is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.repository");
   procedure O is new Broca.Debug.Output (Flag);

   --  Single linked list of all the factories.
   Factories : Factory_Ptr;

   --  Add a new factory to the repository.
   procedure Register (Factory : in Factory_Ptr) is
   begin
      pragma Debug
        (O ("Register new factory " &
            CORBA.To_Standard_String (CORBA.String (Factory.all.Type_Id))));

      Factory.Next := Factories;
      Factories := Factory;
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Type_Id : CORBA.RepositoryId)
     return CORBA.Object.Ref'Class
   is
      Factory   : Factory_Ptr;
      Reference : CORBA.Object.Ref;
   begin
      pragma Debug (O ("Create new object of type " &
                       CORBA.To_Standard_String (CORBA.String (Type_Id))));

      Factory := Factories;
      while Factory /= null loop
         if Factory.Type_Id = Type_Id then
            return Create (Factory);
         end if;
         Factory := Factory.Next;
      end loop;

      --  Return a null object.
      pragma Debug (O ("No factory for this type."));
      CORBA.Object.Set (Reference, null);
      return Reference;

   end Create;

end Broca.Repository;



