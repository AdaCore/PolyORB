------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                     B R O C A . E X C E P T I O N S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Description:
--  Exceptions_Members are handled differently according to the type
--  of the exception:
--   - for System exceptions, it is marshalled in the message
--   - for user exceptions, it is stored in a global stack
--   unless the members is an empty struct, in which case nothing
--   is stored and the Get_Members function created a new
--   object from a derivation od IDL_Exception_Members


with Ada.Exceptions;
with CORBA; use CORBA;

package Droopi.CORBA_P.Exceptions is

   -----------------------------------------
   --  Declarations for user exceptions.  --
   -----------------------------------------

   procedure User_Get_Members
     (Occurrence : in CORBA.Exception_Occurrence;
      Members    : out CORBA.IDL_Exception_Members'Class);
   --  Extract members from an exception occurence

   procedure User_Purge_Members
     (Occurrence : in CORBA.Exception_Occurrence);
   --  Forget exception members associated with an exception occurrence

   procedure User_Raise_Exception
     (Id      : in Ada.Exceptions.Exception_Id;
      Members : in CORBA.IDL_Exception_Members'Class);
   pragma No_Return (User_Raise_Exception);
   --  Raise a user exception

   -------------------------------------------
   --  Declarations for system exceptions.  --
   -------------------------------------------

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out System_Exception_Members);

   function Get_ExcepId_By_RepositoryId
     (RepoId : in Standard.String)
     return Ada.Exceptions.Exception_Id;
   --  return the corresponding Ada Exception_Id for
   --  an IDL repository. Returns Null_Id if RepoId
   --  is unknown.

   function Occurrence_To_Name (Occurrence : CORBA.Exception_Occurrence)
                                return CORBA.RepositoryId;

   ------------------------------------------------------------
   -- conversion between Unsigned_Long and Completion_Status --
   ------------------------------------------------------------

   To_Completion_Status :
     constant array (CORBA.Unsigned_Long range 0 .. 2) of Completion_Status
     := (0 => Completed_Yes, 1 => Completed_No, 2 => Completed_Maybe);

   To_Unsigned_Long :
     constant array (Completion_Status) of CORBA.Unsigned_Long
     := (Completed_Yes => 0, Completed_No => 1, Completed_Maybe => 2);

   ------------------------------------------
   -- Utilities to raise System Exceptions --
   ------------------------------------------

   procedure Raise_Unknown
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Unknown);

   procedure Raise_Bad_Param
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_Param);

   procedure Raise_Marshal
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Marshal);

   procedure Raise_Comm_Failure
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Comm_Failure);

   procedure Raise_Inv_Objref
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Inv_Objref);

   procedure Raise_Object_Not_Exist
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Object_Not_Exist);

   procedure Raise_Obj_Adapter
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Obj_Adapter);

   procedure Raise_Bad_Operation
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_Operation);

   procedure Raise_Transient
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Transient);

   procedure Raise_No_Implement
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_No_Implement);

   procedure Raise_Internal
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Internal);

   procedure Raise_Imp_Limit (Minor : Unsigned_Long := 0;
                              Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Imp_Limit);

   procedure Raise_Bad_Inv_Order
     (Minor  : Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_Inv_Order);

   procedure Raise_Bad_TypeCode
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   ------------------------------------
   --  Exceptions raised by the POA  --
   ------------------------------------

   procedure Raise_Adapter_Already_Exists
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Invalid_Policy
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Wrong_Policy
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Servant_Already_Active
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Object_Already_Active
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Servant_Not_Active
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Object_Not_Active
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Adapter_Inactive
     (Minor  : CORBA.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

end Droopi.CORBA_P.Exceptions;






