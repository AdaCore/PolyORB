with System.Storage_Elements;
with Ada.Task_Attributes;
with Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with CORBA; use CORBA;
with Broca.Marshalling;

package body Broca.Exceptions is
   --  User exception members of a user raised CORBA exception are stored
   --  as a task attribute.
   --  As a result, the user is *not* allowed to save an exception with
   --  primitives such as Save_Occurence.
   package Exception_Attribute is new Ada.Task_Attributes
     (Attribute => IDL_Exception_Members_Acc,
      Initial_Value => null);

   --  Extract members from an exception occurence.
   procedure User_Get_Members
     (Occurrence : Ada.Exceptions.Exception_Occurrence;
      Members : out CORBA.IDL_Exception_Members'Class) is
   begin
      Members := Exception_Attribute.Value.all;
   end User_Get_Members;

   procedure User_Raise_Exception
     (Id : Ada.Exceptions.Exception_Id; Members : IDL_Exception_Members_Acc) is
   begin
      --  FIXME: free previous member.
      Exception_Attribute.Set_Value (Members);

      --  Raise the exception.
      Ada.Exceptions.Raise_Exception (Id);
   end User_Raise_Exception;


   --  Raises the corresponding exception CORBA exception and stores its
   --  member so that it can be retrieved with Get_Members
   procedure Raise_Exception
     (Excp : in Exception_Id; Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Exception);

   procedure Raise_Exception
     (Excp : in Exception_Id; Excp_Memb : in System_Exception_Members)
   is
      Str : String (1 .. 5);
      Val : CORBA.Unsigned_Long;
   begin
      --  Marshall Minor and Completed fields of EXCP_MEMB into a string.
      --  A trivial marshalling is used:
      --  str(1 .. 4) contains the minor, in big endian byte order.
      --  str(5) contains the completed.
      Str (5) := Character'Val (Completion_Status'Pos (Excp_Memb.Completed));
      Val := Excp_Memb.Minor;
      for I in 1 .. 4 loop
         Str (I) := Character'Val (Val / 2 ** 24);
         Val := (Val mod 2 ** 24) * 256;
      end loop;

      --  Raise the exception.
      Ada.Exceptions.Raise_Exception (Excp, Str);

      --  Huh, excp can't be null_id.
      raise Program_Error;
   end Raise_Exception;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members)
   is
      Str : String := Ada.Exceptions.Exception_Message (From);
      Val : Unsigned_Long;
   begin
      --  Check length.
      if Str'Length /= 5 then
         Raise_Bad_Param;
      end if;

      --  Unmarshall completion status.
      --  This can raise constraint_error.
      To.Completed := Completion_Status'Val (Character'Pos (Str (Str'Last)));

      --  Unmarshall minor.
      Val := 0;
      for I in Str'First .. Str'Last - 1 loop
         Val := Val * 256 + Character'Pos (Str (I));
      end loop;
      To.Minor := Val;
   exception
      when Constraint_Error =>
         Raise_Bad_Param;
   end Get_Members;

   --  Raise CORBA.bad_param with minor = 0 and completed = Completed_No.
   procedure Raise_Bad_Param (Status : Completion_Status := Completed_No) is
   begin
      Raise_Exception
        (Bad_Param'Identity,
         System_Exception_Members'(Minor => 0, Completed => Status));
   end Raise_Bad_Param;

   --  Raise CORBA.marshal with minor = 0 and completed = Completed_No.
   procedure Raise_Marshal (Status : Completion_Status := Completed_No) is
   begin
      Raise_Exception
        (Marshal'Identity,
         System_Exception_Members'(Minor => 0, Completed => Status));
   end Raise_Marshal;

   --  Raise CORBA.comm_failure with minor = 0 and completed = Completed_No.
   procedure Raise_Comm_Failure (Status : Completion_Status := Completed_No) is
   begin
      Raise_Exception
        (Comm_Failure'Identity,
         System_Exception_Members'(Minor => 0, Completed => Status));
   end Raise_Comm_Failure;

   --  Raise CORBA.inv_objref with minor = 0 and completed = Completed_No.
   procedure Raise_Inv_Objref (Status : Completion_Status := Completed_No) is
   begin
      Raise_Exception
        (Inv_Objref'Identity,
         System_Exception_Members'(Minor => 0, Completed => Status));
   end Raise_Inv_Objref;

   procedure Raise_Object_Not_Exist
     (Status : Completion_Status := Completed_No) is
   begin
      Raise_Exception
        (Object_Not_Exist'Identity,
         System_Exception_Members'(Minor => 0, Completed => Status));
   end Raise_Object_Not_Exist;

   procedure Raise_Bad_Operation
     (Status : Completion_Status := Completed_No) is
   begin
      Raise_Exception
        (Bad_Operation'Identity,
         System_Exception_Members'(Minor => 0, Completed => Status));
   end Raise_Bad_Operation;

   procedure Raise_Transient
     (Status : Completion_Status := Completed_No) is
   begin
      Raise_Exception
        (Transient'Identity,
         System_Exception_Members'(Minor => 0, Completed => Status));
   end Raise_Transient;

   procedure Raise_Internal
     (Minor : Unsigned_Long; Status : Completion_Status := Completed_No) is
   begin
      Raise_Exception
        (Internal'Identity,
         System_Exception_Members'(Minor => Minor, Completed => Status));
   end Raise_Internal;

   procedure Raise_Obj_Adapter
     (Status : Completion_Status := Completed_No) is
   begin
      Raise_Exception
        (Obj_Adapter'Identity,
         System_Exception_Members'(Minor => 0, Completed => Status));
   end Raise_Obj_Adapter;

   procedure Raise_No_Implement
     (Status : Completion_Status := Completed_No) is
   begin
      Raise_Exception
        (No_Implement'Identity,
         System_Exception_Members'(Minor => 0, Completed => Status));
   end Raise_No_Implement;

   -----------------------------------------------------------------------

   function Occurrence_To_Name (Occurrence : CORBA.Exception_Occurrence)
                                return CORBA.RepositoryId;

   --  System exceptions.
   --  Same as CORBA.To_CORBA_String, but redefined to avoid circular
   --  elaboration.
   function To_RepositoryId (S : in Standard.String) return CORBA.RepositoryId;

   function To_RepositoryId (S : in Standard.String)
                             return CORBA.RepositoryId is
   begin
      return
        CORBA.RepositoryId (Ada.Strings.Unbounded.To_Unbounded_String (S));
   end To_RepositoryId;

   Transient_Name : CORBA.RepositoryId :=
     To_RepositoryId ("IDL:omg.org/CORBA/TRANSIENT:1.0");
   Object_Not_Exist_Name : CORBA.RepositoryId :=
     To_RepositoryId ("IDL:omg.org/CORBA/OBJECT_NOT_EXIST:1.0");

   function Occurrence_To_Name (Occurrence : CORBA.Exception_Occurrence)
                                return CORBA.RepositoryId
   is
      use Ada.Exceptions;
      Id : Exception_Id;
   begin
      Id := Exception_Identity (Occurrence);
      if Id = CORBA.Transient'Identity then
         return Transient_Name;
      elsif Id = CORBA.Object_Not_Exist'Identity then
         return Object_Not_Exist_Name;
      else
         raise Program_Error;
      end if;
   end Occurrence_To_Name;

   procedure Compute_New_Size (Buffer : in out Buffer_Descriptor;
                            Excpt : CORBA.Exception_Occurrence)
   is
      use Broca.Marshalling;
   begin
      Compute_New_Size (Buffer, CORBA.String (Occurrence_To_Name (Excpt)));
      --  Minor.
      Compute_New_Size (Buffer, UL_Size, UL_Size);
      --  Completion status.
      Compute_New_Size (Buffer, UL_Size, UL_Size);
   end Compute_New_Size;

   procedure Marshall (Buffer : in out Buffer_Descriptor;
                       Excpt : CORBA.Exception_Occurrence)
   is
      use Broca.Marshalling;
      Members : System_Exception_Members;
   begin
      Get_Members (Excpt, Members);
      Marshall (Buffer, CORBA.String (Occurrence_To_Name (Excpt)));
      Marshall (Buffer, Members.Minor);
      Marshall
        (Buffer,
         CORBA.Unsigned_Long (Completion_Status'Pos (Members.Completed)));
   end Marshall;

   System_Exception_Header : constant Buffer_Type :=
     (Character'Pos ('I'), Character'Pos ('D'), Character'Pos ('L'),
      Character'Pos (':'),
      Character'Pos ('o'), Character'Pos ('m'), Character'Pos ('g'),
      Character'Pos ('.'),
      Character'Pos ('o'), Character'Pos ('r'), Character'Pos ('g'),
      Character'Pos ('/'),
      Character'Pos ('C'), Character'Pos ('O'), Character'Pos ('R'),
      Character'Pos ('B'), Character'Pos ('A'),
      Character'Pos ('/'));

   procedure Unmarshall_And_Raise (Buffer : in out Buffer_Descriptor) is
      use Broca.Marshalling;
      use Ada.Exceptions;
      Len : CORBA.Unsigned_Long;
      Pos : Buffer_Index_Type;
      Minor : CORBA.Unsigned_Long;
      Status : CORBA.Unsigned_Long;
      Identity : Exception_Id;
   begin
      Unmarshall (Buffer, Len);
      Pos := Buffer.Pos;

      --  Check the prefix.
      if Buffer.Buffer (Pos .. Pos + System_Exception_Header'Length - 1)
        /= System_Exception_Header
      then
         --  The completion status cannot be determined from the buffer,
         --  because the exception might not be a system exception.
         Raise_Marshal (Completed_Maybe);
      end if;
      Pos := Pos + System_Exception_Header'Length;
      Buffer.Pos := Buffer.Pos + Buffer_Index_Type (Len) - 5;
      --  Check version
      if Buffer.Buffer (Buffer.Pos) /= Character'Pos (':') or else
        Buffer.Buffer (Buffer.Pos + 1) /= Character'Pos ('1') or else
        Buffer.Buffer (Buffer.Pos + 2) /= Character'Pos ('.') or else
        Buffer.Buffer (Buffer.Pos + 3) /= Character'Pos ('0') or else
        Buffer.Buffer (Buffer.Pos + 4) /= 0
      then
         Raise_Marshal (Completed_Maybe);
      end if;

      --  Unmarshall the body of the system exception.
      Buffer.Pos := Buffer.Pos + 5;
      Unmarshall (Buffer, Minor);
      Unmarshall (Buffer, Status);

      --  Remove the common prefix and the trailing (version and nul).
      Len := Len - 5 - System_Exception_Header'Length;
      --  Convert the marshalled string into an Ada string.
      declare
         Name_String : String (1 .. Natural (Len));
      begin
         for I in Pos .. Pos + Buffer_Index_Type (Len) - 1 loop
            Name_String (1 + Natural (I - Pos)) :=
              Character'Val (Buffer.Buffer (I));
         end loop;
         Identity := Null_Id;

         --  Associate the exception name with Ada exception identity.
         --  This is done by a selection on the length of the name and then
         --  by comparison.
         case Len is
            when 9 =>
               if Name_String = "TRANSIENT" then
                  Identity := CORBA.Transient'Identity;
               end if;
            when 16 =>
               if Name_String = "OBJECT_NOT_EXIST" then
                  Identity := CORBA.Object_Not_Exist'Identity;
               end if;
            when others =>
               null;
         end case;
      end;

      if Identity = Null_Id then
         --  If not found, this is a marshal error.
         Identity := CORBA.Marshal'Identity;
         Minor := 0;
         Status := Completion_Status'Pos (Completed_Maybe);
      end if;

      --  Raise the exception.
      Raise_Exception
        (Identity,
         System_Exception_Members'(Minor, Completion_Status'Val (Status)));
   end Unmarshall_And_Raise;

   procedure Raise_With_Address (Id : Ada.Exceptions.Exception_Id;
                                 Addr : System.Address)
   is
      use System.Storage_Elements;
      Val : Integer_Address;
      Str : String (1 .. 8);
   begin
      Val := To_Integer (Addr);
      for I in 1 .. 8 loop
         Str (I) := Character'Val (Val mod 256);
         Val := Val / 256;
      end loop;
      Ada.Exceptions.Raise_Exception (Id, Str);

      --  Huh, excp can't be null_id.
      Broca.Exceptions.Raise_Internal (51);
   end Raise_With_Address;

   procedure Get_Member (Occurrence : Ada.Exceptions.Exception_Occurrence;
                         Addr : out System.Address)
   is
      use System.Storage_Elements;
      Val : Integer_Address;
      Str : String := Ada.Exceptions.Exception_Message (Occurrence);
   begin
      if Str'Length /= 8 then
         Raise_Bad_Param;
      end if;
      Val := 0;
      for I in reverse 1 .. 8 loop
         Val := Val * 256 +  Character'Pos (Str (I));
      end loop;
      Addr := To_Address (Val);
   exception
      when Constraint_Error =>
         Raise_Bad_Param;
   end Get_Member;
end Broca.Exceptions;
