with Ada.Finalization;

with AdaBroker;
with AdaBroker.OmniORB;

package CORBA.Object is

   type Ref is tagged private;

   function Is_Nil  (Self : in Ref) return CORBA.Boolean;
   function Is_Null (Self : in Ref) return CORBA.Boolean renames Is_Nil;

   procedure Release (Self : in out Ref);

   function Is_A
     (Self            : in Ref;
      Logical_Type_Id : in CORBA.String)
      return CORBA.Boolean;
   --  Return True if Self type is Logical_Type_Id (a Repository_Id)
   --  or one of its descendants.

   function Non_Existent
     (Self : in Ref)
      return CORBA.Boolean;
   --  Return True when the implementation referenced by this proxy
   --  object does not exist.

   function Is_Equivalent
     (Self  : in Ref;
      Other : in Ref)
      return CORBA.Boolean;
   --  Return True when both objects point to the same distant
   --  implementation.

   function Hash
     (Self    : in Ref;
      Maximum : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  Return a hash value for object. Not implemented yet.

   function Get_Implementation
     (Self : in Ref'Class)
      return AdaBroker.OmniORB.OmniObject_Ptr;

   Nil_Ref : constant Ref;

private

   type Ref is new Ada.Finalization.Controlled with record
      OmniObj : AdaBroker.OmniORB.OmniObject_Ptr := null;
   end record;

   Repository_Id : CORBA.String
     := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Object:1.0");

   procedure Initialize (Self : in out Ref);
   --  Set OmniObject to null.

   procedure Adjust (Self : in out Ref);
   --  Duplicate underlying OmniObject

   procedure Finalize (Self : in out Ref);
   --  Release underlying OmniObject

   Nil_Ref : constant Ref
     := (Ada.Finalization.Controlled with OmniObj => null);

end CORBA.Object;
