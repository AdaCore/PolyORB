------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            C O R B A . O R B                             --
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

--  The standard CORBA ORB interface.

--  $Id$

with Sequences.Unbounded;

with CORBA.Context;
with CORBA.ExceptionList;
with CORBA.NVList;
with CORBA.Object;

with PolyORB.References;

package CORBA.ORB is

   pragma Elaborate_Body;

   package Octet_Sequence is
      new Sequences.Unbounded (Octet);

   type ServiceDetail is record
      Service_Detail_Type : ServiceDetailType;
      Service_Detail      : Octet_Sequence.Sequence;
   end record;

   package IDL_Sequence_ServiceOption is new
     Sequences.Unbounded (ServiceOption);

   package IDL_Sequence_ServiceDetail is new
     Sequences.Unbounded (ServiceDetail);

   type ServiceInformation is record
      service_options : IDL_Sequence_ServiceOption.Sequence;
      service_details : IDL_Sequence_ServiceDetail.Sequence;
   end record;

   type ObjectId is new CORBA.String;

   package IDL_Sequence_ObjectId is new
     Sequences.Unbounded (ObjectId);

   type ObjectIdList is new IDL_Sequence_ObjectId.Sequence;

   function  Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
     return CORBA.String;

   procedure String_To_Object
     (From : in     CORBA.String;
      To   : in out CORBA.Object.Ref'Class);

   --  Dynamic Invocation related operations

   procedure Create_List
     (Count    : in     CORBA.Long;
      New_List :    out CORBA.NVList.Ref);

   procedure Create_List
     (New_List : out CORBA.ExceptionList.Ref);

   --  ??? Requires CORBA.OperationDef

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

   --  initial reference operations

   function List_Initial_Services return ObjectIdList;

   procedure Register_Initial_Reference
     (Identifier : ObjectId;
      Ref        : CORBA.Object.Ref);

   function Resolve_Initial_References
     (Identifier : ObjectId)
     return CORBA.Object.Ref;

   --  ??? Requires CORBA.StructMemberSeq

   --    function  create_struct_tc
   --      (Id      : in CORBA.RepositoryId;
   --       Name    : in CORBA.Identifier;
   --       Members : in CORBA.StructMemberSeq)
   --      return CORBA.TypeCode.Object;

   --  ??? Requires CORBA.EnumMemberSeq

   --    function  create_enum_tc
   --      (Id      : in CORBA.RepositoryId;
   --       Name    : in CORBA.Identifier;
   --       Members : in CORBA.EnumMemberSeq)
   --      return CORBA.TypeCode.Object;

   function  create_alias_tc
     (Id            : in CORBA.RepositoryId;
      Name          : in CORBA.Identifier;
      Original_Type : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object;

   --  ??? Requires CORBA.StructMemberSeq

   --    function  create_exception_tc
   --      (Id      : in CORBA.RepositoryId;
   --       Name    : in CORBA.Identifier;
   --       Members : in CORBA.StructMemberSeq)
   --      return CORBA.TypeCode.Object;

   function  create_interface_tc
     (Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
     return CORBA.TypeCode.Object;

   function  create_string_tc
     (Bound : in CORBA.Unsigned_Long)
     return CORBA.TypeCode.Object;

   function  create_wstring_tc
     (Bound : in CORBA.Unsigned_Long)
     return CORBA.TypeCode.Object;

   function  create_fixed_tc
     (IDL_Digits : in CORBA.Unsigned_Short;
      scale      : in CORBA.Short)
     return CORBA.TypeCode.Object;

   function  create_sequence_tc
     (Bound        : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object;

   function  create_recursive_sequence_tc
     (Bound  : in CORBA.Unsigned_Long;
      Offset : in CORBA.Unsigned_Long)
     return CORBA.TypeCode.Object;

   function create_array_tc
     (Length       : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object;

   function create_native_tc
     (Id   : in RepositoryId;
      Name : in Identifier)
     return CORBA.TypeCode.Object;

   --  Thread related operations

   function Work_Pending return Boolean;
   procedure Perform_Work;
   procedure Shutdown (Wait_For_Completion : in Boolean);
   procedure Run;

   --  Policy related operations

   --  ??? in the spec this is declared a function.

   procedure Create_Policy
     (The_Type : in PolicyType;
      Val      : Any);

   --  The following subprograms are not in CORBA spec.

   procedure Initialize
     (ORB_Name : in Standard.String);

   function Create_Reference
     (Object : in CORBA.Object.Ref;
      Typ : in Standard.String)
     return PolyORB.References.Ref;
   --  Create an object reference that designates object Oid
   --  of type Typ within this ORB.

end CORBA.ORB;
