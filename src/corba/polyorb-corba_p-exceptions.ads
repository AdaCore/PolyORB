------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . C O R B A _ P . E X C E P T I O N S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Exceptions management for the CORBA Applicative Personality
--  of PolyORB.

--  Description:
--  Exceptions_Members are handled differently according to the type
--  of the exception:
--   - for System exceptions, it is marshalled in the message
--   - for User exceptions, it is stored in a global stack
--   unless the members is an empty struct, in which case nothing
--   is stored and the Get_Members function created a new
--   object from a derivation of IDL_Exception_Members

--  $Id: //droopi/main/src/corba/polyorb-corba_p-exceptions.ads#7 $

with Ada.Exceptions;

with CORBA; use CORBA;

with PolyORB.Any;
with PolyORB.Types;

package PolyORB.CORBA_P.Exceptions is

   -----------------------------------------
   --  Declarations for user exceptions.  --
   -----------------------------------------

   procedure User_Get_Members
     (Occurrence : in Ada.Exceptions.Exception_Occurrence;
      Members    : out CORBA.IDL_Exception_Members'Class);
   --  Extract members from an exception occurence

   procedure User_Purge_Members
     (Occurrence : in Ada.Exceptions.Exception_Occurrence);
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
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members);

   function Occurrence_To_Name
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
      return CORBA.RepositoryId;

   ------------------------------------------------------------
   -- Conversion between Unsigned_Long and Completion_Status --
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

   type Raise_From_Any_Procedure is access procedure
     (Occurrence : CORBA.Any);

   procedure Register_Exception
     (TC     : in CORBA.TypeCode.Object;
      Raiser : in Raise_From_Any_Procedure);
   --  Associate the TypeCode for a user-defined exception with
   --  a procedure that raises an occurrence of that exception,
   --  given an Any with that TypeCode.
   --  (When a client creates a request, it is his responsability
   --  to provide the list of typecodes of potential exceptions,
   --  so the generic middleware can unmarshall occurrences and
   --  store them into an Any. It is then the responsibility of
   --  the application layer -- here, the CORBA PortableServer --
   --  to map the Any back to whatever representation is relevant
   --  in the application personality: here, raising a language
   --  exception with proper members.

   procedure Raise_From_Any (Occurrence : Any.Any);
   pragma No_Return (Raise_From_Any);

   function System_Exception_TypeCode
     (Name : PolyORB.Types.RepositoryId)
     return Any.TypeCode.Object;
   --  Return the TypeCode corresponding to the indicated
   --  system exception name.

end PolyORB.CORBA_P.Exceptions;
