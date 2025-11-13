------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O L Y O R B . E R R O R S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Errors management subsystem

with PolyORB.Utils.Unchecked_Deallocation;
with PolyORB.Smart_Pointers;
with PolyORB.Types;

package PolyORB.Errors is

   pragma Preelaborate;

   ------------------------
   -- Exceptions Members --
   ------------------------

   --  A PolyORB error is notionally equivalent to a CORBA exception.
   --  It is composed of:
   --   - Exception Id,
   --   - Exception Member.

   type Exception_Members is abstract tagged null record;
   --  Base type for all PolyORB exception members. A member is a
   --  record attached to an exception that allows the programmer to
   --  pass arguments when an exception is raised.

   type Exception_Members_Access is access all Exception_Members'Class;

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Exception_Members'Class, Name => Exception_Members_Access);

   -----------------------
   -- Completion_Status --
   -----------------------

   type Completion_Status is
     (Completed_Yes,
      Completed_No,
      Completed_Maybe);
   --  Characterize the completion state of the execution process when
   --  systeme exception has been raised.

   --  Null_Members

   type Null_Members is new Exception_Members with null record;

   Null_Member : constant Null_Members
     := Null_Members'(Exception_Members with null record);

   --  System_Exception_Members

   type System_Exception_Members is new Exception_Members with record
      Minor     : PolyORB.Types.Unsigned_Long;
      Completed : Completion_Status;
   end record;

   --  InvalidPolicy_Members

   type InvalidPolicy_Members is new Exception_Members with record
      Index : PolyORB.Types.Unsigned_Short;
   end record;

   --  ForwardRequest_Members

   type ForwardRequest_Members is new Exception_Members with record
      Forward_Reference : PolyORB.Smart_Pointers.Ref;
   end record;

   --  ForwardRequestPerm_Members

   type ForwardRequestPerm_Members is new Exception_Members with record
      Forward_Reference : PolyORB.Smart_Pointers.Ref;
   end record;

   --  NeedsAddressingMode_Members

   type Addressing_Mode is (Key, Profile, Reference);

   type NeedsAddressingMode_Members is new Exception_Members with record
      Mode : Addressing_Mode;
   end record;

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
      Initialize_E,               --  ORB initialization failure
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

      --  Special case for processing PortableServer's and
      --  PortableInterceptor's ForwardRequest exception.

      ForwardRequest_E,

      --  Special error code for Fault Tolerant permanent location forwarding

      ForwardRequestPerm_E,

      --  Special error code for requesting GIOP addressing mode

      NeedsAddressingMode_E,

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
      Invalid_Object_Id_E,

      --  Group exception
      NotAGroupObject_E
      );

   subtype ORB_System_Error       is Error_Id
     range Unknown_E .. Bad_Qos_E;
   subtype POA_Error              is Error_Id
     range AdapterAlreadyExists_E .. WrongPolicy_E;
   subtype POAManager_Error        is Error_Id
     range AdapterInactive_E .. AdapterInactive_E;
   subtype PolyORB_Internal_Error is Error_Id
     range Invalid_Object_Id_E .. Error_Id'Last;

   ----------------------
   -- Error management --
   ----------------------

   type Error_Container is record
      Kind   : Error_Id := No_Error;
      Member : Exception_Members_Access;
   end record;

   function Found (Error : Error_Container) return Boolean;
   --  True iff Error is not No_Error

   procedure Throw
     (Error  : in out Error_Container;
      Kind   : Error_Id;
      Member : Exception_Members'Class);
   --  Generates an error with Kind and Member information

   procedure Catch (Error : in out Error_Container);
   --  Acknowledge Error and reset its content

   function Is_Error (Error : Error_Container) return Boolean;
   --  True iff Error is not No_Error

   ------------------
   -- Exception Id --
   ------------------

   --  An exception Id has the following form:
   --  NameSpace:Root'Separator' .. Version

   PolyORB_Exc_NameSpace : constant String;
   --  PolyORB exceptions namespace

   PolyORB_Exc_Root      : constant String;
   --  PolyORB exceptions root

   PolyORB_Exc_Separator : constant String;
   --  PolyORB exceptions separator

   PolyORB_Exc_Prefix    : constant String;
   --  Concantenation of PolyORB_Exc_NameSpace, PolyORB_Root and
   --  PolyORB_Separator

   PolyORB_Exc_Version   : constant String;
   --  PolyORB exceptions version

private

   PolyORB_Exc_NameSpace : constant String := "INTERNAL:";
   PolyORB_Exc_Root      : constant String := "POLYORB";
   PolyORB_Exc_Separator : constant String := "/";
   PolyORB_Exc_Prefix    : constant String
     := PolyORB_Exc_NameSpace
     & PolyORB_Exc_Root
     & PolyORB_Exc_Separator;

   PolyORB_Exc_Version   : constant String := ":1.0";

end PolyORB.Errors;
