--   Droopi.Representations.CDR; use Droopi.Representations.CDR;
with Droopi.Buffers;             use Droopi.Buffers;
with Ada.Streams;

package body CORBA.POA_Types is

   ---------------
   -- Create_Id --
   ---------------

   function Create_Id
     (Name             : in CORBA.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in CORBA.Short)
     return Unmarshalled_Oid
   is
      U_Oid : Unmarshalled_Oid_Ptr;
   begin
      U_Oid := new Unmarshalled_Oid'(Name,
                                     System_Generated,
                                     Persistency_Flag);
      return U_Oid.all;
   end Create_Id;

   ---------------
   -- Create_Id --
   ---------------

   function Create_Id
     (Name             : in CORBA.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in CORBA.Short)
     return Object_Id
   is
   begin
      return Create_Id (Name, System_Generated, Persistency_Flag);
   end Create_Id;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall (Oid : Object_Id) return Unmarshalled_Oid
   is
   begin
      return Unmarshall (Oid);
   end Unmarshall;

   --------------
   -- Marshall --
   --------------

   function Marshall (U_Oid : Unmarshalled_Oid) return Object_Id
   is
      Buffer, Tmp_Buffer : aliased Buffer_Type;
      --      Oid                : Ada.Streams.Stream_Element_Array;
   begin
      --       Marshall (Buffer'Access, U_Oid.Id);
      --       Marshall (Tmp_Buffer'Access, U_Oid.System_Generated);
      --       Prepend (Tmp_Buffer, Buffer'Access);
      --       Marshall (Tmp_Buffer'Access, U_Oid.Persistency_Flag);
      --       Prepend (Tmp_Buffer, Buffer'Access);
      --
      --       Oid := Unmarshall (Buffer);
      --       Release_Contents (Buffer);
      --       Release_Contents (Tmp_Buffer);

      --  Uncomment when droopi.representations.cdr works
      return Marshall (U_Oid);
   end Marshall;

end CORBA.POA_Types;
