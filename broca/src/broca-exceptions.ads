with System;
with Ada.Exceptions;
with CORBA; use CORBA;
with Broca.Types; use Broca.Types;

package Broca.Exceptions is
   -----------------------------------------
   --  Declarations for user exceptions.  --
   -----------------------------------------

   type IDL_Exception_Members_Acc is
      access all CORBA.IDL_Exception_Members'Class;

   --  Extract members from an exception occurence.
   procedure User_Get_Members
     (Occurrence : Ada.Exceptions.Exception_Occurrence;
      Members : out CORBA.IDL_Exception_Members'Class);

   --  Raise an user exception.
   procedure User_Raise_Exception
     (Id : Ada.Exceptions.Exception_Id; Members : IDL_Exception_Members_Acc);

   -------------------------------------------
   --  Declarations for system exceptions.  --
   -------------------------------------------

   --  Raise CORBA.bad_param with minor = 0.
   procedure Raise_Bad_Param (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_Param);

   --  Raise CORBA.marshal with minor = 0.
   procedure Raise_Marshal (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Marshal);

   --  Raise CORBA.comm_failure with minor = 0.
   procedure Raise_Comm_Failure (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Comm_Failure);

   --  Raise CORBA.inv_objref with minor = 0.
   procedure Raise_Inv_Objref (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Inv_Objref);

   --  Raise CORBA.object_not_exist with minor = 0.
   procedure Raise_Object_Not_Exist
     (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Object_Not_Exist);

   --  Raise CORBA.obj_adapter with minor = 0.
   procedure Raise_Obj_Adapter
     (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Obj_Adapter);

   --  Raise CORBA.bad_operation with minor = 0.
   procedure Raise_Bad_Operation
     (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_Operation);

   --  Raise CORBA.transient with minor = 0.
   procedure Raise_Transient
     (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Transient);

   --  Raise CORBA.no_implement with minor = 0.
   procedure Raise_No_Implement
     (Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_No_Implement);

   --  Raise CORBA.internal
   procedure Raise_Internal
     (Minor : Unsigned_Long; Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Internal);

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members);

   --  Only for a system exception.
   procedure Marshall_Size (Buffer : in out Buffer_Descriptor;
                            Excpt : CORBA.Exception_Occurrence);
   procedure Marshall (Buffer : in out Buffer_Descriptor;
                       Excpt : CORBA.Exception_Occurrence);

   procedure Unmarshall_And_Raise (Buffer : in out Buffer_Descriptor);
   pragma No_Return (Unmarshall_And_Raise);

   procedure Raise_With_Address (Id : Ada.Exceptions.Exception_Id;
                                 Addr : System.Address);
   pragma No_Return (Raise_With_Address);

   procedure Get_Member (Occurrence : Ada.Exceptions.Exception_Occurrence;
                         Addr : out System.Address);
end Broca.Exceptions;
