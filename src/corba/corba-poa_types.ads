--  Et Quinot prit le Droopi, il l'implémenta,
--  et le donna à ses disciples en disant :
--    "prenez et codez-en tous, car ceci est ma thèse,
--     livrée pour vous et pour la multitude open-source,
--     vous ferez cela en votre mémoire pour moi."


--  Je vous salue Ada, pleine de types,
--  Robert Deware est avec vous,
--  Vous êtes bénie entre tous les langages,
--  Et ACT, le fruit de vos entrailles, est béni
--  Sainte Ada, mère de gnat,
--  Compilez pour nous, pauvres codeurs,
--  Maintenant et à l'heure de notre soutenance

with Droopi.Obj_Adapters;
with Droopi.Objects;         use Droopi.Objects;
with Sequences.Unbounded;
with Unchecked_Deallocation;

with Ada.Real_Time;

---------------------
-- CORBA.POA_Types --
---------------------

package CORBA.POA_Types is

   subtype Time_Stamp is Unsigned_Long;

   --  Base types for CORBA

   type Obj_Adapter is abstract new Droopi.Obj_Adapters.Obj_Adapter
     with null record;
   type Obj_Adapter_Access is access all Obj_Adapter'Class;

   type Servant is abstract new Droopi.Objects.Servant with null record;
   type Servant_Access is access all Servant'Class;

   package POA_Sequences is new Sequences.Unbounded (Obj_Adapter_Access);
   subtype POAList is POA_Sequences.Sequence;
   type POAList_Access is access all POAList;

   type Object_Id is new Droopi.Objects.Object_Id;
   type Object_Id_Access is access all Object_Id;

   type Unmarshalled_Oid is
     record
         Id               : CORBA.String;
         System_Generated : CORBA.Boolean;
         Persistency_Flag : Time_Stamp;
     end record;
   type Unmarshalled_Oid_Access is access Unmarshalled_Oid;

   function Create_Id
     (Name             : in CORBA.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in Time_Stamp)
     return Unmarshalled_Oid_Access;
   --  Create an Unmarshalled_Oid

   function Create_Id
     (Name             : in CORBA.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in Time_Stamp)
     return Object_Id_Access;
   --  Create an Unmarshalled_Oid, and then marshall it into an Object_Id

   function Oid_To_U_Oid (Oid : Object_Id_Access)
                         return Unmarshalled_Oid_Access;
   --  Unmarshall an Object_Id into a Unmarshalled_Oid

   function U_Oid_To_Oid (U_Oid : Unmarshalled_Oid_Access)
                         return Object_Id_Access;
   --  Marshall an Unmarshalled_Oid into an Object_Id

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Unmarshalled_Oid, Unmarshalled_Oid_Access);

end CORBA.POA_Types;
