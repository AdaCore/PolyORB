with Droopi.Obj_Adapters;
with Droopi.Objects;        use Droopi.Objects;
with Unchecked_Deallocation;

---------------------
-- CORBA.POA_Types --
---------------------

package CORBA.POA_Types is

   type Obj_Adapter is abstract new Droopi.Obj_Adapters.Obj_Adapter with
      record
         Name         : CORBA.String;
      end record;
   type Obj_Adapter_Ptr is access Obj_Adapter;
   --  The POA object

   type Unmarshalled_Oid is
     record
         Id               : CORBA.String;
         System_Generated : CORBA.Boolean;
         Persistency_Flag : CORBA.Short;
         --  ??? How do we implement the PERSISTENT policy?
     end record;
   type Unmarshalled_Oid_Ptr is access Unmarshalled_Oid;
   --  The unmarshalled Object_Id

   function Create_Id
     (Name             : in CORBA.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in CORBA.Short)
     return Unmarshalled_Oid;
   --  Create an Unmarshalled_Oid

   function Create_Id
     (Name             : in CORBA.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in CORBA.Short)
     return Object_Id;
   --  Create an Unmarshalled_Oid, and then marshall it into an Object_Id

   function Unmarshall (Oid : Object_Id) return Unmarshalled_Oid;
   --  Unmarshall an Object_Id into a Unmarshalled_Oid

   function Marshall (U_Oid : Unmarshalled_Oid) return Object_Id;
   --  Marshall an Unmarshalled_Oid into an Object_Id

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Unmarshalled_Oid, Unmarshalled_Oid_Ptr);

end CORBA.POA_Types;
