--  Base types for the Portable Object Adapter.

--  $Id$

with Ada.Unchecked_Deallocation;

with CORBA;

with Droopi.Obj_Adapters;
with Droopi.Objects;         use Droopi.Objects;
with Droopi.Any;
with Droopi.Any.NVList;
with Droopi.Requests;
with Droopi.Storage_Pools;
with Droopi.Types; use Droopi.Types;
with Sequences.Unbounded;

---------------------
-- Droopi.POA_Types --
---------------------

package Droopi.POA_Types is

   pragma Elaborate_Body;

   Invalid_Object_Id : exception renames Droopi.Obj_Adapters.Invalid_Object_Id;
   Invalid_Method    : exception renames Droopi.Obj_Adapters.Invalid_Method;

   subtype Time_Stamp is Unsigned_Long;

   --  Base types for CORBA

   type Obj_Adapter is abstract new Droopi.Obj_Adapters.Obj_Adapter
      with null record;
   type Obj_Adapter_Access is access all Obj_Adapter'Class;

   type Parameter_Profile_Description is
     access function (Method : Droopi.Requests.Operation_Id)
                     return Droopi.Any.NVList.Ref;

   type Result_Profile_Description is
     access function (Method : Droopi.Requests.Operation_Id)
                     return Droopi.Any.Any;

   type Interface_Description is record
      External_Name : Identifier;
      --  External representation of the interface name.

      PP_Desc : Parameter_Profile_Description;
      RP_Desc : Result_Profile_Description;
   end record;

   type Servant is abstract new Droopi.Objects.Servant with
      record
         If_Desc : Interface_Description;
         --  Description of the most derived interface supported
         --  by this servant.
      end record;
   type Servant_Access is access all Servant'Class;

   package POA_Sequences is new Sequences.Unbounded (Obj_Adapter_Access);
   subtype POAList is POA_Sequences.Sequence;
   type POAList_Access is access all POAList;

   subtype Object_Id is Droopi.Objects.Object_Id;
   subtype Object_Id_Access is Droopi.Objects.Object_Id_Access;
   function "=" (X, Y : Object_Id_Access) return Boolean
     renames Droopi.Objects."=";

   type Unmarshalled_Oid is
     record
         Id               : Types.String;
         System_Generated : Boolean;
         Persistency_Flag : Time_Stamp;
         Creator          : Types.String;
     end record;
   type Unmarshalled_Oid_Access is access Unmarshalled_Oid;
   for Unmarshalled_Oid_Access'Storage_Pool use Storage_Pools.Debug_Pool;

   function "=" (Left, Right : in Servant) return Standard.Boolean
      is abstract;

   function "=" (Left, Right : in Unmarshalled_Oid) return Standard.Boolean;

   function Image
     (Oid : Object_Id) return Types.String;
   --  For debugging purposes.

   function Create_Id
     (Name             : in Types.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in Time_Stamp;
      Creator          : in Types.String)
     return Unmarshalled_Oid_Access;
   --  Create an Unmarshalled_Oid

   function Create_Id
     (Name             : in Types.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in Time_Stamp;
      Creator          : in Types.String)
     return Object_Id_Access;
   --  Create an Unmarshalled_Oid, and then marshall it into an Object_Id

   function Oid_To_U_Oid
     (Oid : Object_Id_Access)
     return Unmarshalled_Oid_Access;

   function Oid_To_U_Oid
     (Oid : Object_Id)
     return Unmarshalled_Oid_Access;
   --  Unmarshall an Object_Id into a Unmarshalled_Oid

   function U_Oid_To_Oid
     (U_Oid : Unmarshalled_Oid_Access)
     return Object_Id_Access;
   --  Marshall an Unmarshalled_Oid into an Object_Id

   procedure Free (X : in out Droopi.POA_Types.Object_Id_Access)
     renames Droopi.Objects.Free;

   procedure Free is new Ada.Unchecked_Deallocation
     (Unmarshalled_Oid, Unmarshalled_Oid_Access);

end Droopi.POA_Types;
