with CORBA;

generic

   type Operation_Type is private;
   --  Typically an access to subprogram type

package Broca.Operation_Store is

   procedure Register_Operation
     (RepoId : in CORBA.RepositoryId;
      Op : in Operation_Type);
   --  Register an operation for a RepositoryId

   procedure Register_Operation
     (RepoId : in Standard.String;
      Op : in Operation_Type);
   --  Register an operation for a RepositoryId

   function Get_Operation
     (RepoId : in Standard.String)
      return Operation_Type;
   --  Retrieves the stored operation for this type.
   --  Raises CORBA.Internal if not found

   function Get_Operation
     (RepoId : in CORBA.RepositoryId)
      return Operation_Type;
   --  Retrieves the stored operation for this type.
   --  Raises CORBA.Internal if not found

end Broca.Operation_Store;
