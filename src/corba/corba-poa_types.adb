with Droopi.Representations.CDR; use Droopi.Representations.CDR;
with Droopi.Buffers;             use Droopi.Buffers;
with Ada.Streams;

package body CORBA.POA_Types is

   use Ada.Streams;

   ---------------
   -- Create_Id --
   ---------------

   function Create_Id
     (Name             : in CORBA.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in CORBA.Long)
     return Unmarshalled_Oid_Access
   is
      U_Oid : Unmarshalled_Oid_Access;
   begin
      U_Oid := new Unmarshalled_Oid'(Name,
                                     System_Generated,
                                     Persistency_Flag);
      return U_Oid;
   end Create_Id;

   ---------------
   -- Create_Id --
   ---------------

   function Create_Id
     (Name             : in CORBA.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in CORBA.Long)
     return Object_Id_Access
   is
   begin
      return Create_Id (Name, System_Generated, Persistency_Flag);
      --  ??? needs to be implemented
   end Create_Id;

   ------------------
   -- Oid_To_U_Oid --
   ------------------

   function Oid_To_U_Oid (Oid : Object_Id_Access)
                         return Unmarshalled_Oid_Access
   is
      U_Oid            : Unmarshalled_Oid_Access;
      Stream           : aliased Stream_Element_Array
        := Stream_Element_Array (Oid.all);
      Buffer           : aliased Buffer_Type;

      Id               : CORBA.String;
      System_Generated : CORBA.Boolean := True;
      Persistency_Flag : CORBA.Long    := 1;
   begin

      Decapsulate (Stream'Access, Buffer'Access);
      Id               := Unmarshall (Buffer'Access);
      System_Generated := Unmarshall (Buffer'Access);
      Persistency_Flag := Unmarshall (Buffer'Access);
      Release_Contents (Buffer);

      U_Oid := new Unmarshalled_Oid'(Id, System_Generated, Persistency_Flag);

      return U_Oid;
   end Oid_To_U_Oid;

   ------------------
   -- U_Oid_To_Oid --
   ------------------

   function U_Oid_To_Oid (U_Oid : Unmarshalled_Oid_Access)
                         return Object_Id_Access
   is
      Buffer             : Buffer_Access := new Buffer_Type;
      Initial_Position   : Stream_Element_Offset;
      Oid                : Object_Id_Access;
      Hello              : CORBA.String := To_CORBA_String ("Hello");
   begin
      Start_Encapsulation (Buffer);
      Marshall (Buffer, U_Oid.Id);
      Marshall (Buffer, U_Oid.System_Generated);
      Marshall (Buffer, U_Oid.Persistency_Flag);

      Oid := new Object_Id'(Object_Id (Encapsulate (Buffer)));
      Release (Buffer);

      return Oid;
   end U_Oid_To_Oid;
end CORBA.POA_Types;
