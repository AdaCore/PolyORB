------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O R T A B L E I N T E R C E P T O R                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

with Ada.Exceptions;

with CORBA;
with CORBA.Object;
with CORBA.Sequences.Unbounded;

package PortableInterceptor is

   --  Implementation Notes: this type temporary replace CORBA::StringSeq type.
   package IDL_Sequence_String is new CORBA.Sequences.Unbounded (CORBA.String);

   --  Implementation Notes: this type temporary replace CORBA::OctetSeq type.
   package IDL_Sequence_Octet is new CORBA.Sequences.Unbounded (CORBA.Octet);

   --  ForwardRequest exception

   ForwardRequest : exception;

   type ForwardRequest_Members is new CORBA.IDL_Exception_Members with record
      Forward : CORBA.Object.Ref;
   end record;

   procedure Get_Members
     (From : in     Ada.Exceptions.Exception_Occurrence;
      To   :    out ForwardRequest_Members);

   --  ReplyStatus type and constants

   type ReplyStatus is new CORBA.Short;

   Successful       : constant ReplyStatus := 0;
   System_Exception : constant ReplyStatus := 1;
   User_Exception   : constant ReplyStatus := 2;
   Location_Forward : constant ReplyStatus := 3;
   Transport_Retry  : constant ReplyStatus := 4;
   Unknown          : constant ReplyStatus := 5;

   --  SlotId type

   type SlotId is new CORBA.Unsigned_Long;

   --  InvalidSlot exception

   InvalidSlot : exception;

   type InvalidSlot_Members is
      new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : in     Ada.Exceptions.Exception_Occurrence;
      To   :    out InvalidSlot_Members);

   --  ServerId type

   type ServerId is new CORBA.String;

   --  ORBId type

   type ORBId is new CORBA.String;

   --  AdapterName type

   type AdapterName is new IDL_Sequence_String.Sequence;

   --  ObjectId type

   type ObjectId is new IDL_Sequence_Octet.Sequence;

   --  AdapterId type (PolyORB extension)

   type AdapterId is new IDL_Sequence_Octet.Sequence;

   --  XXX TODO ObjectReferenceTemplateSeq type
   --  typedef sequence<ObjectReferenceTemplate> ObjectReferenceTemplateSeq;

   --  AdapterManagerId type

   type AdapterManagerId is new CORBA.String;

   --  AdapterState type

   type AdapterState is new CORBA.Short;

   Holding      : constant AdapterState := 0;
   Active       : constant AdapterState := 1;
   Discarding   : constant AdapterState := 2;
   Inactive     : constant AdapterState := 3;
   Non_Existent : constant AdapterState := 4;

   --  Repository_Ids

   Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor:1.0";

   ForwardRequest_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/ForwardRequest:1.0";

   ReplyStatus_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/ReplyStatus:1.0";

   SlotId_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/SlotId:1.0";

   InvalidSlot_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/InvalidSlot:1.0";

   ServerId_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/ServerId:1.0";

   ORBId_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/ORBId:1.0";

   AdapterName_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/AdapterName:1.0";

   ObjectId_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/ObjectId:1.0";

   ObjectReferenceTemplateSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/ObjectReferenceTemplateSeq:1.0";

   AdapterManagerId_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/AdapterManagerId:1.0";

   AdapterState_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/AdapterState:1.0";

end PortableInterceptor;
