--  This package is a generic hashtable to
--  store the operations associated with
--  a value_impl.Object tagged type.
--  It is not implemented as a hashtable but as a
--  linked list

with Ada.Tags;

generic

   type Operation_Type is private;
   --  usually an access to subprogram type

package Broca.Value.Operation_Store is

   --  Register an operation for a tagged type
   --  deriving from CORBA.Value.Impl_Base
   procedure Register_Operation (T : in Ada.Tags.Tag;
                                 Op : in Operation_Type);

   --  Retrieves the stored operation for this type.
   --  Raises CORBA.Internal if not found
   function Get_Operation (T : in Ada.Tags.Tag)
                           return Operation_Type;

end Broca.Value.Operation_Store;
