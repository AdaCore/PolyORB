with Droopi.Representations.CDR; use Droopi.Representations.CDR;
with Droopi.Buffers;             use Droopi.Buffers;
with Droopi.Types;
with Ada.Streams;
with Ada.Unchecked_Conversion;

package body CORBA.POA_Types is

   use Ada.Streams;

   ----------
   -- Left --
   ----------

   function "=" (Left, Right : in Unmarshalled_Oid) return Standard.Boolean
   is
   begin
      if Left.Id = Right.Id
        and then Left.System_Generated = Right.System_Generated
        and then Left.Persistency_Flag = Right.Persistency_Flag
      then
         return True;
      end if;
      return False;
   end "=";

   -----------
   -- Image --
   -----------

   function Image (Oid : Object_Id) return String
   is
   begin
      return To_Corba_String (Droopi.Objects.To_String
                              (Droopi.Objects.Object_Id (Oid)));
   end Image;

   ---------------
   -- Create_Id --
   ---------------

   function Create_Id
     (Name             : in CORBA.String;
      System_Generated : in CORBA.Boolean;
      Persistency_Flag : in Time_Stamp)
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
      Persistency_Flag : in Time_Stamp)
     return Object_Id_Access
   is
      U_Oid : Unmarshalled_Oid_Access;
   begin
      U_Oid := Create_Id (Name, System_Generated, Persistency_Flag);
      return U_Oid_To_Oid (U_Oid);
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

      Id               : Droopi.Types.String;
      System_Generated : Droopi.Types.Boolean;
      Persistency_Flag : Droopi.Types.Unsigned_Long;
   begin

      Decapsulate (Stream'Access, Buffer'Access);
      Id               := Unmarshall (Buffer'Access);
      System_Generated := Unmarshall (Buffer'Access);
      Persistency_Flag := Unmarshall (Buffer'Access);
      Release_Contents (Buffer);

      U_Oid := new Unmarshalled_Oid'(CORBA.String (Id),
                                     CORBA.Boolean (System_Generated),
                                     Time_Stamp (Persistency_Flag));
      return U_Oid;
   end Oid_To_U_Oid;

   ------------------
   -- Oid_To_U_Oid --
   ------------------

   function Oid_To_U_Oid (Oid : Object_Id)
                         return Unmarshalled_Oid_Access
   is
      Oid_Access : Object_Id_Access;
      U_Oid      : Unmarshalled_Oid_Access;
   begin
      Oid_Access := new Object_Id'(Oid);
      Oid_Access.all := Oid;
      U_Oid := Oid_To_U_Oid (Oid_Access);
      Free (Oid_Access);
      return U_Oid;
      --  ??? Does this work? Not tested yet.
   end Oid_To_U_Oid;

   ------------------
   -- U_Oid_To_Oid --
   ------------------

   function U_Oid_To_Oid (U_Oid : Unmarshalled_Oid_Access)
                         return Object_Id_Access
   is
      Buffer             : Buffer_Access := new Buffer_Type;
      Oid                : Object_Id_Access;
   begin

      Start_Encapsulation (Buffer);
      Marshall (Buffer, Droopi.Types.String (U_Oid.Id));
      Marshall (Buffer, Droopi.Types.Boolean (U_Oid.System_Generated));
      Marshall (Buffer, Droopi.Types.Unsigned_Long (U_Oid.Persistency_Flag));

      Oid := new Object_Id'(Object_Id (Encapsulate (Buffer)));
      Release (Buffer);
      return Oid;
   end U_Oid_To_Oid;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out CORBA.POA_Types.Object_Id_Access)
   is
      Y : Droopi.Objects.Object_Id_Access
        := Droopi.Objects.Object_Id_Access (X);
   begin
      Droopi.Objects.Free (Y);
   end Free;

end CORBA.POA_Types;
