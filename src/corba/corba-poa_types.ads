--  Et Quinot prit le Droopi, il l'impl�menta,
--  et le donna � ses disciples en disant :
--    "prenez et codez-en tous, car ceci est ma th�se,
--     livr�e pour vous et pour la multitude open-source,
--     vous ferez cela en votre m�moire pour moi."


--  Je vous salue Ada, pleine de types,
--  Robert Deware est avec vous,
--  Vous �tes b�nie entre tous les langages,
--  Et ACT, le fruit de vos entrailles, est b�ni
--  Sainte Ada, m�re de gnat,
--  Compilez pour nous, pauvres codeurs,
--  Maintenant et � l'heure de notre soutenance

with Droopi.Obj_Adapters;
with Droopi.Objects;         use Droopi.Objects;
with Droopi.Any;
with Droopi.Any.NVList;
with Droopi.Requests;
with Sequences.Unbounded;

---------------------
-- CORBA.POA_Types --
---------------------

package CORBA.POA_Types is

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
      PP_Desc : Parameter_Profile_Description;
      RP_Desc : Result_Profile_Description;
   end record;

   type Servant is abstract new Droopi.Objects.Servant with
      record
         If_Desc : Interface_Description;
      end record;
   type Servant_Access is access all Servant'Class;

   package POA_Sequences is new Sequences.Unbounded (Obj_Adapter_Access);
   subtype POAList is POA_Sequences.Sequence;
   type POAList_Access is access all POAList;

   subtype Object_Id is Droopi.Objects.Object_Id;
   type Object_Id_Access is access all Object_Id;

   type Unmarshalled_Oid is
     record
         Id               : CORBA.String;
         System_Generated : CORBA.Boolean;
         Persistency_Flag : Time_Stamp;
         Creator          : CORBA.String;
     end record;
   type Unmarshalled_Oid_Access is access Unmarshalled_Oid;

   function "=" (Left, Right : in Servant) return Standard.Boolean
      is abstract;

   function "=" (Left, Right : in Unmarshalled_Oid) return Standard.Boolean;

   function Image (Oid : Object_Id) return String;
   --  For debugging purposes.

   function Create_Id
     (Name             : in CORBA.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in Time_Stamp;
      Creator          : in CORBA.String)
     return Unmarshalled_Oid_Access;
   --  Create an Unmarshalled_Oid

   function Create_Id
     (Name             : in CORBA.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in Time_Stamp;
      Creator          : in CORBA.String)
     return Object_Id_Access;
   --  Create an Unmarshalled_Oid, and then marshall it into an Object_Id

   function Oid_To_U_Oid (Oid : Object_Id_Access)
                         return Unmarshalled_Oid_Access;
   function Oid_To_U_Oid (Oid : Object_Id)
                         return Unmarshalled_Oid_Access;
   --  Unmarshall an Object_Id into a Unmarshalled_Oid

   function U_Oid_To_Oid (U_Oid : Unmarshalled_Oid_Access)
                         return Object_Id_Access;
   --  Marshall an Unmarshalled_Oid into an Object_Id

   procedure Free (X : in out CORBA.POA_Types.Object_Id_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Unmarshalled_Oid, Unmarshalled_Oid_Access);

end CORBA.POA_Types;
