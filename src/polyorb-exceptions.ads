------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . E X C E P T I O N S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
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

   function To_Any
     (Item : Completion_Status)
     return Any.Any;

   function TC_Completion_Status
     return PolyORB.Any.TypeCode.Object;
   --  The typecode for standard enumeration type completion_status.

   ------------------------
   -- Exceptions Members --
   ------------------------

   --  A PolyORB exception is notionally equivalent to a CORBA exception.
   --  It is composed by
   --   - Exception Id,
   --   - Exception Member.

   type Exception_Members is abstract tagged null record;
   --  Base type for all PolyORB exception members. A member is a record
   --  attached to an exception that allows the programmer to pass
   --  arguments when an exception is raised. The default Member record is
   --  abstract and empty but all other records will inherit from it.

   type Exception_Members_Access is access all Exception_Members'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Exception_Members'Class, Exception_Members_Access);

   --  Null_Members

   type Null_Members is new Exception_Members with null record;

   function To_Any
     (Name   : Standard.String;
      Member : Null_Members)
     return PolyORB.Any.Any;

   Null_Member : constant Null_Members
     := Null_Members'(Exception_Members with null record);

   --  System_Exception_Members

   type System_Exception_Members is new Exception_Members with record
      Minor     : PolyORB.Types.Unsigned_Long;
      Completed : Completion_Status;
   end record;

   function System_Exception_TypeCode
     (Name : Standard.String)
     return PolyORB.Any.TypeCode.Object;
   --  Return the TypeCode corresponding to the indicated
   --  system exception name.

   function To_Any
     (Name   : Standard.String;
      Member : System_Exception_Members)
     return PolyORB.Any.Any;

   --  InvalidPolicy_Members

   type InvalidPolicy_Members is new Exception_Members with record
      Index : PolyORB.Types.Short;
   end record;

   ------------------
   -- Exception Id --
   ------------------

   --  An exception Id has the following form:
   --  NameSpace:Root'Separator' .. Version

   PolyORB_Exc_NameSpace : constant String;
   --  PolyORB exceptions namespace.

   PolyORB_Exc_Root      : constant String;
   --  PolyORB exceptions root.

   PolyORB_Exc_Separator : constant String;
   --  PolyORB exceptions separator.

   PolyORB_Exc_Prefix    : constant String;
   --  Concantenation of PolyORB_Exc_NameSpace, PolyORB_Root and
   --  PolyORB_Separator.

   PolyORB_Exc_Version   : constant PolyORB.Types.String;
   --  PolyORB exceptions version.

   ----------------
   -- ORB Errors --
   ----------------

   type Error_Id is
     (
      No_Error,                 --  no error

      --  One to one mapping of CORBA System exceptions.

      Unknown_E,                  --  unknown exception
      Bad_Param_E,                --  an invalid parameter was passed
      No_Memory_E,                --  dynamic memory allocation failure
      Imp_Limit_E,                --  violated implementation limit
      Comm_Failure_E,             --  communication failure
      Inv_Objref_E,               --  invalid object reference
      No_Permission_E,            --  no permission for attempted op.
      Internal_E,                 --  ORB internal error
      Marshal_E,                  --  error marshalling param/result
      Initialization_Failure_E,   --  ORB initialization failure
      No_Implement_E,             --  operation impleme. unavailable
      Bad_TypeCode_E,             --  bad typecode
      Bad_Operation_E,            --  invalid operation
      No_Resources_E,             --  insufficient resources for req.
      No_Response_E,              --  response to request not available
      Persist_Store_E,            --  persistent storage failure
      Bad_Inv_Order_E,            --  routine invocations out of order
      Transient_E,                --  transient failure - reissue request
      Free_Mem_E,                 --  cannot free memory
      Inv_Ident_E,                --  invalid identifier syntax
      Inv_Flag_E,                 --  invalid flag was specified
      Intf_Repos_E,               --  error accessing intf. repository
      Bad_Context_E,              --  error processing context object
      Obj_Adapter_E,              --  failure detected by object adapter
      Data_Conversion_E,          --  data conversion error
      Object_Not_Exist_E,         --  non-existent object, delete ref.
      Transaction_Required_E,     --  transaction required
      Transaction_Rolledback_E,   --  transaction rolled back
      Invalid_Transaction_E,      --  invalid transaction
      Inv_Policy_E,               --  invalid policy
      Codeset_Incompatible_E,     --  incompatible code set
      Rebind_E,                   --  rebind needed
      Timeout_E,                  --  operation timed out
      Transaction_Unavailable_E,  --  no transaction
      Transaction_Mode_E,         --  invalid transaction mode
      Bad_Qos_E,                  --  bad quality of service

      --  One to one mapping of POA exceptions.

      AdapterAlreadyExists_E,
      AdapterNonExistent_E,
      InvalidPolicy_E,
      NoServant_E,
      ObjectAlreadyActive_E,
      ObjectNotActive_E,
      ServantAlreadyActive_E,
      ServantNotActive_E,
      WrongAdapter_E,
      WrongPolicy_E,

      --  One to one mapping of POA Manager exceptions.

      AdapterInactive_E,

      --  PolyORB internal errors.
      Invalid_Object_Id_E

      );

   subtype ORB_System_Error       is Error_Id
     range Unknown_E .. Bad_Qos_E;
   subtype POA_Error              is Error_Id
     range AdapterAlreadyExists_E .. WrongPolicy_E;
   subtype POAManager_Error        is Error_Id
     range AdapterInactive_E .. AdapterInactive_E;
   subtype PolyORB_Internal_Error is Error_Id
     range Invalid_Object_Id_E .. Error_Id'Last;

   ---------------------
   -- User exceptions --
   ---------------------

   procedure User_Get_Members
     (Occurrence : in  Ada.Exceptions.Exception_Occurrence;
      Members    : out PolyORB.Exceptions.Exception_Members'Class);
   --  Extract members from a 'User' exception occurence

   procedure User_Purge_Members
     (Occurrence : in Ada.Exceptions.Exception_Occurrence);
   --  Forget exception members associated with an exception occurrence

   procedure User_Raise_Exception
     (Id      : in Ada.Exceptions.Exception_Id;
      Members : in PolyORB.Exceptions.Exception_Members'Class);
   pragma No_Return (User_Raise_Exception);
   --  Raise a user exception with the specified members.

   procedure Raise_User_Exception_From_Any
     (Repository_Id : PolyORB.Types.RepositoryId;
      Occurence     : PolyORB.Any.Any);

   type Raise_From_Any_Procedure is access procedure
     (Occurrence : PolyORB.Any.Any);

   procedure Default_Raise_From_Any
     (Occurrence : PolyORB.Any.Any);

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

   ---------------------------------
   -- Exception utility functions --
   ---------------------------------

   function Occurrence_To_Name
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
      return PolyORB.Types.RepositoryId;

   function Exception_Name
     (Repository_Id : Standard.String)
      return Standard.String;
   --  Return the name of an exception from its repository ID.

   function Get_ExcepId_By_RepositoryId
     (RepoId  : Standard.String)
      return Ada.Exceptions.Exception_Id;
   --  Return the corresponding Ada Exception_Id for
   --  a repository id.

   -----------------------------------------------
   -- PolyORB Internal Error handling functions --
   -----------------------------------------------

   type Error_Container is record
      Kind   : Error_Id := No_Error;
      Member : Exception_Members_Access;
   end record;

   function Found
     (Error : Error_Container)
     return Boolean;
   --  True iff Error is not null.

   procedure Throw
     (Error  : in out Error_Container;
      Kind   : in     Error_Id;
      Member : in     Exception_Members'Class);
   --  Generates an error whith Kind and Member information.

   procedure Catch
     (Error : in out Error_Container);
   --  Acknowledge 'Error' and reset its content.

   function Is_Error
     (Error : in Error_Container)
     return Boolean;
   --  True iff Error is not No_Error;

   function Error_To_Any
     (Error : in Error_Container)
     return PolyORB.Any.Any;

private

   PolyORB_Exc_NameSpace : constant String := "INTERNAL:";
   PolyORB_Exc_Root      : constant String := "POLYORB";
   PolyORB_Exc_Separator : constant String := "/";
   PolyORB_Exc_Prefix    : constant String
     := PolyORB_Exc_NameSpace
     & PolyORB_Exc_Root
     & PolyORB_Exc_Separator;

   PolyORB_Exc_Version   : constant PolyORB.Types.String
     := PolyORB.Types.To_PolyORB_String (":1.0");

end PolyORB.Exceptions;
