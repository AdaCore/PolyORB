------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            C O R B A . O R B                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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

--  The standard CORBA ORB interface.

--  $Id$

with CORBA.Context;
with CORBA.ExceptionList;
with CORBA.NVList;
with CORBA.Object;
with CORBA.Policy;
with CORBA.Sequences.Unbounded;

with PolyORB.References;

package CORBA.ORB is

   pragma Elaborate_Body;

   --  ORB initialisation

   type ORBid is new CORBA.String;

   package IDL_Sequence_String is
      new CORBA.Sequences.Unbounded (CORBA.String);
   --  Implementation Note: the IDL-to-Ada mapping specification does
   --  not define the formal type for package instantiation, we retain
   --  CORBA.String.

   type Arg_List is new IDL_Sequence_String.Sequence;

   function Command_Line_Arguments return Arg_List;

   procedure Init
     (ORB_Indentifier : in     ORBid;
      Argv            : in out Arg_List);
   --  Implementation Note:
   --  * the CORBA specification defines this procedure in the module
   --  CORBA.  the IDL-to-Ada mapping declares it in this package to
   --  avoid circular dependences on the ORBid type.
   --
   --  * the CORBA specification states Argv is an inout parameter,
   --  the IDL-to-Ada mapping specification indicates it should be an
   --  in parameter. The IDL-to-Ada defines a default parameter, yet
   --  this is not allowed for in out parmaters by the Ada Reference
   --  Manual. PolyORB follows the semantics prescribed by the CORBA
   --  specification.

   package Octet_Sequence is
      new CORBA.Sequences.Unbounded (Octet);

   type ServiceDetail is record
      Service_Detail_Type : ServiceDetailType;
      Service_Detail      : Octet_Sequence.Sequence;
   end record;

   package IDL_Sequence_ServiceOption is new
     CORBA.Sequences.Unbounded (ServiceOption);

   package IDL_Sequence_ServiceDetail is new
     CORBA.Sequences.Unbounded (ServiceDetail);

   type ServiceInformation is record
      service_options : IDL_Sequence_ServiceOption.Sequence;
      service_details : IDL_Sequence_ServiceDetail.Sequence;
   end record;

   type ObjectId is new CORBA.String;

   package IDL_Sequence_ObjectId is new
     CORBA.Sequences.Unbounded (ObjectId);

   type ObjectIdList is new IDL_Sequence_ObjectId.Sequence;

   InvalideName : exception;

   function Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
     return CORBA.String;
   --  Convert reference to IOR

   procedure String_To_Object
     (From : in     CORBA.String;
      To   : in out CORBA.Object.Ref'Class);

   --  Dynamic Invocation related operations

   procedure Create_List
     (Count    : in     CORBA.Long;
      New_List :    out CORBA.NVList.Ref);
   --  Implementation Note: the parameter Count is only a hint.
   --  In this implementation, it is ignored.

   procedure Create_List
     (New_List : out CORBA.ExceptionList.Ref);

   --  XXX Requires CORBA.OperationDef defined in COS IR

   --    procedure Create_Operation_List
   --      (Oper     : in     CORBA.OperationDef.Ref'Class;
   --       New_List :    out CORBA.NVList.Object);

   function Get_Default_Context
     return CORBA.Context.Ref;

   --  Service information operations

   procedure Get_Service_Information
     (Service_Type        : in     CORBA.ServiceType;
      Service_Information :    out ServiceInformation;
      Returns             :    out CORBA.Boolean);

   function List_Initial_Services return ObjectIdList;

   --  Initial reference operations

   procedure Register_Initial_Reference
     (Identifier : ObjectId;
      Ref        : CORBA.Object.Ref);

   function Resolve_Initial_References
     (Identifier : ObjectId)
     return CORBA.Object.Ref;

   --  Type code creation operations

   function Create_Alias_Tc
     (Id            : in CORBA.RepositoryId;
      Name          : in CORBA.Identifier;
      Original_Type : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object;

   function Create_Interface_Tc
     (Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
     return CORBA.TypeCode.Object;

   function Create_String_Tc
     (Bound : in CORBA.Unsigned_Long)
     return CORBA.TypeCode.Object;

   function Create_Wstring_Tc
     (Bound : in CORBA.Unsigned_Long)
     return CORBA.TypeCode.Object;

   function Create_Fixed_Tc
     (IDL_Digits : in CORBA.Unsigned_Short;
      scale      : in CORBA.Short)
     return CORBA.TypeCode.Object;

   function Create_Sequence_Tc
     (Bound        : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object;

   function Create_Recursive_Sequence_Tc
     (Bound  : in CORBA.Unsigned_Long;
      Offset : in CORBA.Unsigned_Long)
     return CORBA.TypeCode.Object;

   function Create_Array_Tc
     (Length       : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object;

   function Create_Native_Tc
     (Id   : in RepositoryId;
      Name : in Identifier)
     return CORBA.TypeCode.Object;

   --  The following functions require CORBA.*MemberSeq sequence types
   --  and are therefore defined only in CORBA.ORB.TypeCode (which is
   --  part of the Interface Repository implementation):
   --    function  create_struct_tc
   --    function  create_enum_tc
   --    function  create_exception_tc

   --  Thread related operations

   function Work_Pending return Boolean;

   procedure Perform_Work;

   procedure Run;

   procedure Shutdown (Wait_For_Completion : in Boolean);

   --  Policy related operations

   function Create_Policy
     (The_Type : in PolicyType;
      Val      :    Any)
     return CORBA.Policy.Ref;

   --  The following subprograms are not in CORBA spec.

   procedure Initialize (ORB_Name : in Standard.String);
   --  Implementation Note: this procedure is deprecated, use
   --  CORBA.ORB.Init instead

   function Create_Reference
     (Object : in CORBA.Object.Ref;
      Typ    : in Standard.String)
     return PolyORB.References.Ref;
   --  Create an object reference that designates object Oid
   --  of type Typ within this ORB.

   -------------------------------------
   -- CORBA.ORB Exceptions Management --
   -------------------------------------

   --  InvalidName_Members

   type InvalidName_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidName_Members);

   procedure Raise_InvalidName
     (Excp_Memb : in InvalidName_Members);
   pragma No_Return (Raise_InvalidName);

end CORBA.ORB;
