package body Broca.Value.Value_Skel is

   --  Is_A operation for a top level valuetype
   function Is_A (Type_Id : in CORBA.RepositoryId) return CORBA.Boolean is
   begin
      return Broca.Repository.Is_Equivalent
        (Type_Id,
         RepositoryId (CORBA.String'(To_CORBA_String
                                     ("IDL:omg.org/CORBA/VALUEBASE:1.0"))));
   end;

begin

   Is_A_Store.Register_Operation
     (CORBA.Value.Impl_Base'Tag,
      Is_A'Access);

end Broca.Value.Value_Skel;
