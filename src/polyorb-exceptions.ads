------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . E X C E P T I O N S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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

--  Exceptions and Errors management subsystem.

--  PolyORB distinguishes errors and exceptions:
--
--  A non null 'error' means that a wrong execution occurs within
--  PolyORB's core middleware or one of its personalities.
--
--  An 'exception' is one possible result of the execution of a
--  personality-specific function or procedure. It is either raised
--  within application personality context, or returned in the request
--  response message. User and System exceptions follow CORBA definition
--  of exceptions kinds.
--
--  When raised, 'exception' is built from 'error' information,
--  translated to personality specific context.

--  PolyORB's core middleware should not raise exceptions, except Ada
--  standard exceptions ad defined in the Ada Reference Manual. It
--  should use instead 'error' as defined in the Error_Container type.

--  $Id$

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Types;

package PolyORB.Exceptions is

   ----------------
   -- Exceptions --
   ----------------

   --  A PolyORB exception is notionally equivalent to a CORBA exception.
   --  It is composed by
   --   - Exception Id,
   --   - Exception Member.

   type Exception_Members is abstract tagged null record;
   --  Base type for all PolyORB exception members. A member is a record
   --  attached to an exception that allows the programmer to pass
   --  arguments when an exception is raised. The default Member record is
   --  abstract and empty but all other records will inherit from it.

   --  Note:
   --  Exceptions_Members are handled differently according to the type
   --  of the exception:
   --   - for System exceptions, it is marshalled in the message
   --   - for User exceptions, it is stored in a global stack
   --   unless the members is an empty struct, in which case nothing
   --   is stored and the Get_Members function created a new
   --   object from a derivation of Exception_Members

   type Exception_Members_Access is access all Exception_Members'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Exception_Members'Class, Exception_Members_Access);

   subtype Exception_Occurrence is Ada.Exceptions.Exception_Occurrence;

   ------------------
   -- Exception Id --
   ------------------

   --  An exception Id has the following form:
   --  NameSpace:Root'Separator' .. Version

   PolyORB_Exc_NameSpace : constant String;
   --  PolyORB exceptions namespace.

   PolyORB_Root          : constant String;
   --  PolyORB exceptions root.

   PolyORB_Separator     : constant String;
   --  PolyORB exceptions separator.

   PolyORB_Prefix        : constant String;
   --  Concantenation of PolyORB_Exc_NameSpace, PolyORB_Root and
   --  PolyORB_Separator.

   PolyORB_Exc_Version   : constant PolyORB.Types.String;
   --  PolyORB exceptions version.

   ---------------------
   -- User exceptions --
   ---------------------

   procedure User_Get_Members
     (Occurrence : in  Ada.Exceptions.Exception_Occurrence;
      Members    : out Exception_Members'Class);
   --  Extract members from a 'User' exception occurence

   procedure User_Purge_Members
     (Occurrence : in Ada.Exceptions.Exception_Occurrence);
   --  Forget exception members associated with an exception occurrence

   procedure User_Raise_Exception
     (Id      : in Ada.Exceptions.Exception_Id;
      Members : in Exception_Members'Class);
   pragma No_Return (User_Raise_Exception);
   --  Raise a user exception

   procedure Raise_User_Exception_From_Any
     (Repository_Id : PolyORB.Types.RepositoryId;
      Occurence     : PolyORB.Any.Any);

   -----------------------
   -- Completion_Status --
   -----------------------

   type Completion_Status is
     (Completed_Yes,
      Completed_No,
      Completed_Maybe);
   --  Characterize the completion state of the execution process when
   --  systeme exception has been raised.

   To_Completion_Status :
     constant array (PolyORB.Types.Unsigned_Long range 0 .. 2)
         of Completion_Status
     := (0 => Completed_Yes, 1 => Completed_No, 2 => Completed_Maybe);

   To_Unsigned_Long :
     constant array (Completion_Status) of PolyORB.Types.Unsigned_Long
     := (Completed_Yes => 0, Completed_No => 1, Completed_Maybe => 2);

   function From_Any
     (Item : PolyORB.Any.Any)
     return Completion_Status;

   function TC_Completion_Status
     return PolyORB.Any.TypeCode.Object;
   --  The typecode for standard enumeration type completion_status.

   -----------------------
   -- System exceptions --
   -----------------------

   type System_Exception_Members is new Exception_Members with record
      Minor     : PolyORB.Types.Unsigned_Long;
      Completed : Completion_Status;
   end record;
   --  Member type for System exceptions.

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members);
   --  Extract members from a 'System' exception occurence

   procedure Raise_System_Exception
     (Excp      : in Ada.Exceptions.Exception_Id;
      Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_System_Exception);
   --  Raise the corresponding PolyORB system exception, and store its
   --  members for later retrieval by Get_Members.

   function System_Exception_TypeCode
     (Name : Standard.String)
     return PolyORB.Any.TypeCode.Object;
   --  Return the TypeCode corresponding to the indicated
   --  system exception name.

   function System_Exception_To_Any
     (E : Ada.Exceptions.Exception_Occurrence)
     return PolyORB.Any.Any;

   procedure Raise_System_Exception_From_Any
     (System_Id  : Ada.Exceptions.Exception_Id;
      Occurrence : PolyORB.Any.Any);

   function Is_System_Exception
     (Name : String)
     return Boolean;

   ---------------------------------
   -- System Exception definition --
   ---------------------------------

   --  One to one mapping to CORBA System exceptions.

   Unknown                : exception;
   Bad_Param              : exception;
   No_Memory              : exception;
   Imp_Limit              : exception;
   Comm_Failure           : exception;
   Inv_Objref             : exception;
   No_Permission          : exception;
   Internal               : exception;
   Marshal                : exception;
   Initialization_Failure : exception;
   No_Implement           : exception;
   Bad_TypeCode           : exception;
   Bad_Operation          : exception;
   No_Resources           : exception;
   No_Response            : exception;
   Persist_Store          : exception;
   Bad_Inv_Order          : exception;
   Transient              : exception;
   Free_Mem               : exception;
   Inv_Ident              : exception;
   Inv_Flag               : exception;
   Intf_Repos             : exception;
   Bad_Context            : exception;
   Obj_Adapter            : exception;
   Data_Conversion        : exception;
   Object_Not_Exist       : exception;
   Transaction_Required   : exception;
   Transaction_Rolledback : exception;
   Invalid_Transaction    : exception;
   Adapter_Already_Exists : exception;
   Invalid_Policy         : exception;
   Wrong_Policy           : exception;
   Servant_Already_Active : exception;
   Object_Already_Active  : exception;
   Servant_Not_Active     : exception;
   Object_Not_Active      : exception;
   Adapter_Inactive       : exception;

   type Unknown_Members         is new
     System_Exception_Members with null record;

   type Bad_Param_Members       is new
     System_Exception_Members with null record;

   type No_Memory_Members       is new
     System_Exception_Members with null record;

   type Imp_Limit_Members       is new
     System_Exception_Members with null record;

   type Comm_Failure_Members    is new
     System_Exception_Members with null record;

   type Inv_Objref_Members      is new
     System_Exception_Members with null record;

   type No_Permission_Members   is new
     System_Exception_Members with null record;

   type Internal_Members        is new
     System_Exception_Members with null record;

   type Marshal_Members         is new
     System_Exception_Members with null record;

   type Initialization_Failure_Members is new
     System_Exception_Members with null record;

   type No_Implement_Members    is new
     System_Exception_Members with null record;

   type Bad_Typecode_Members    is new
     System_Exception_Members with null record;

   type Bad_Operation_Members   is new
     System_Exception_Members with null record;

   type No_Resources_Members    is new
     System_Exception_Members with null record;

   type No_Response_Members     is new
     System_Exception_Members with null record;

   type Persist_Store_Members   is new
     System_Exception_Members with null record;

   type Bad_Inv_Order_Members   is new
     System_Exception_Members with null record;

   type Transient_Members       is new
     System_Exception_Members with null record;

   type Free_Mem_Members        is new
     System_Exception_Members with null record;

   type Inv_Ident_Members       is new
     System_Exception_Members with null record;

   type Inv_Flag_Members        is new
     System_Exception_Members with null record;

   type Intf_Repos_Members      is new
     System_Exception_Members with null record;

   type Bad_Context_Members     is new
     System_Exception_Members with null record;

   type Obj_Adapter_Members     is new
     System_Exception_Members with null record;

   type Data_Conversion_Members is new
     System_Exception_Members with null record;

   type Object_Not_Exist_Members is new
     System_Exception_Members with null record;

   type Transaction_Required_Members is new
     System_Exception_Members with null record;

   type Transaction_Rolledback_Members is new
     System_Exception_Members with null record;

   type Invalid_Transaction_Members    is new
     System_Exception_Members with null record;

   type Adapter_Already_Exists_Members is new
     System_Exception_Members with null record;

   type Invalid_Policy_Members is new
     System_Exception_Members with null record;

   ------------------------------------------
   -- Utilities to raise System Exceptions --
   ------------------------------------------

   procedure Raise_Unknown
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Unknown);

   procedure Raise_Bad_Param
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_Param);

   procedure Raise_Marshal
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Marshal);

   procedure Raise_Comm_Failure
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Comm_Failure);

   procedure Raise_Inv_Objref
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Inv_Objref);

   procedure Raise_Object_Not_Exist
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Object_Not_Exist);

   procedure Raise_Obj_Adapter
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Obj_Adapter);

   procedure Raise_Bad_Operation
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_Operation);

   procedure Raise_Transient
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Transient);

   procedure Raise_No_Implement
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_No_Implement);

   procedure Raise_Internal
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Internal);

   procedure Raise_Imp_Limit
     (Minor : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Imp_Limit);

   procedure Raise_Bad_Inv_Order
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_Inv_Order);

   procedure Raise_Bad_TypeCode
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   ------------------------------------
   --  Exceptions raised by the POA  --
   ------------------------------------

   procedure Raise_Adapter_Already_Exists
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Invalid_Policy
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Wrong_Policy
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Servant_Already_Active
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Object_Already_Active
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Servant_Not_Active
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Object_Not_Active
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Adapter_Inactive
     (Minor  : PolyORB.Types.Unsigned_Long := 0;
      Status : Completion_Status := Completed_No);
   pragma No_Return (Raise_Bad_TypeCode);

   ---------------------------------
   -- Exception utility functions --
   ---------------------------------

   type Raise_From_Any_Procedure is access procedure
     (Occurrence : PolyORB.Any.Any);

   procedure Default_Raise_From_Any
     (Occurrence : Any.Any);

   procedure Register_Exception
     (TC     : in PolyORB.Any.TypeCode.Object;
      Raiser : in Raise_From_Any_Procedure);
   --  Associate the TypeCode for a user-defined exception with
   --  a procedure that raises an occurrence of that exception,
   --  given an Any with that TypeCode.
   --  (When a client creates a request, it is his responsability
   --  to provide the list of typecodes of potential exceptions,
   --  so the generic middleware can unmarshall occurrences and
   --  store them into an Any. It is then the responsibility of
   --  the application layer -- eg. the CORBA PortableServer --
   --  to map the Any back to whatever representation is relevant
   --  in the application personality: here, raising a language
   --  exception with proper members.

   function Occurrence_To_Name
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
      return PolyORB.Types.RepositoryId;

   function Exception_Name
     (Repository_Id : Standard.String)
      return Standard.String;
   --  Return the name of an exception from its repository ID.

   type Exception_Info is record
      TC     : PolyORB.Any.TypeCode.Object;
      Raiser : Raise_From_Any_Procedure;
   end record;

   function Find_Exception_Info
     (For_Exception : PolyORB.Types.RepositoryId)
     return Exception_Info;
   --  Return Exception_Info associated to 'For_Exception'.

   function Get_ExcepId_By_RepositoryId
     (RepoId  : Standard.String)
      return Ada.Exceptions.Exception_Id;
   --  Return the corresponding Ada Exception_Id for
   --  a repository id.

   -----------------------------------------------
   -- PolyORB Internal Error handling functions --
   -----------------------------------------------

   type Error_Container is private;

   function Found
     (Error : Error_Container)
     return Boolean;
   --  True iff Error is not null.

   procedure Throw
     (Error  : in out Error_Container;
      Kind   : in     Ada.Exceptions.Exception_Id;
      Member : in     Exception_Members_Access);
   --  Generates an error whith Kind and Member information.

   procedure Catch
     (Error : in out Error_Container);
   --  Acknowledge 'Error' and reset its content.

   procedure Raise_From_Error
     (Error : in out Error_Container);
   pragma No_Return (Raise_From_Error);
   --  Raise an exception from the data in 'Error'

private

   PolyORB_Exc_NameSpace : constant String := "INTERNAL:";
   PolyORB_Root          : constant String := "POLYORB";
   PolyORB_Separator     : constant String := "/";
   PolyORB_Prefix        : constant String
     := PolyORB_Exc_NameSpace
     & PolyORB_Root
     & PolyORB_Separator;

   PolyORB_Exc_Version   : constant PolyORB.Types.String
     := PolyORB.Types.To_PolyORB_String (":1.0");

   type Error_Container is record
      Kind   : Ada.Exceptions.Exception_Id := Ada.Exceptions.Null_Id;
      Member : Exception_Members_Access;
   end record;

end PolyORB.Exceptions;
