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

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall (Oid : Object_Id_Access) return Unmarshalled_Oid_Access
   is
      U_Oid            : Unmarshalled_Oid_Access;
      Stream           : Stream_Element_Array
        := Stream_Element_Array (Oid.all);
--       Encapsule        : aliased Encapsulation
--         := Encapsulation (Stream_Element_Array (Oid.all));

      Buffer           : aliased Buffer_Type;
      Initial_Position : Stream_Element_Offset;
      Id               : CORBA.String;
      System_Generated : CORBA.Boolean := True;
      Persistency_Flag : CORBA.Long    := 1;
   begin
--      Start_Encapsulation (Buffer'Access);

      Initial_Position := CDR_Position (Buffer'Access);

      --       Decapsulate (Encapsule'Access, Buffer'Access);
      Marshall (Buffer'Access, Stream);
      Set_Initial_Position (Buffer'Access, Initial_Position);
--       Persistency_Flag := Unmarshall (Buffer'Access);
--       System_Generated := Unmarshall (Buffer'Access);
      Id               := Unmarshall (Buffer'Access);
      U_Oid := new Unmarshalled_Oid'(Id, System_Generated, Persistency_Flag);

      return U_Oid;
   end Unmarshall;

   --------------
   -- Marshall --
   --------------

   function Marshall (U_Oid : Unmarshalled_Oid_Access) return Object_Id_Access
   is
      Buffer, Tmp_Buffer : aliased Buffer_Type;
      Initial_Position   : Stream_Element_Offset;
      Oid                : Object_Id_Access;
      Hello              : CORBA.String := To_CORBA_String ("Hello");
   begin
--       Initial_Position := CDR_Position (Buffer'Access);
--       Start_Encapsulation (Buffer'Access);
--       --      Start_Encapsulation (Tmp_Buffer'Access);

      Initial_Position := CDR_Position (Buffer'Access);
      Marshall (Buffer'Access, U_Oid.Id);
--       Marshall (Tmp_Buffer'Access, U_Oid.System_Generated);
--       Prepend  (Tmp_Buffer, Buffer'Access);
--       Marshall (Tmp_Buffer'Access, U_Oid.Persistency_Flag);
--       Prepend  (Tmp_Buffer, Buffer'Access);
--       Set_Initial_Position (Buffer'Access, Initial_Position);

      declare
         Stream : Stream_Element_Array := Unmarshall (Buffer'Access);
      begin
         Oid     := new Object_Id'(Object_Id (Stream));
      end;

      Release_Contents (Buffer);
--       Release_Contents (Tmp_Buffer);
      return Oid;
   end Marshall;
end CORBA.POA_Types;
