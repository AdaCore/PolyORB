------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O R T A B L E S E R V E R . H E L P E R                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2008, Free Software Foundation, Inc.          --
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
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC version 2.3.0w.
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks ("NM32766");

with PolyORB.Any;
with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;

package PortableServer.Helper is

   function Unchecked_To_Ref
     (The_Ref : CORBA.Object.Ref'Class) return PortableServer.POA_Forward.Ref;

   function To_Ref
     (The_Ref : CORBA.Object.Ref'Class) return PortableServer.POA_Forward.Ref;

   TC_POA : CORBA.TypeCode.Object;

   TC_IDL_SEQUENCE_PortableServer_POA_Forward : CORBA.TypeCode.Object;

   TC_POAList : CORBA.TypeCode.Object;

   TC_ObjectId : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return PortableServer.ObjectId;

   function To_Any
     (Item : PortableServer.ObjectId) return CORBA.Any;

   TC_ForwardRequest : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return PortableServer.ForwardRequest_Members;

   function To_Any
     (Item : PortableServer.ForwardRequest_Members) return CORBA.Any;

   procedure Raise_ForwardRequest
     (Members : ForwardRequest_Members);
   pragma No_Return (Raise_ForwardRequest);
   function Wrap (X : access PortableServer.ThreadPolicyValue) return PolyORB.Any.Content'Class;

   TC_ThreadPolicyValue : CORBA.TypeCode.Object;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.ThreadPolicyValue;

   function From_Any (Item : CORBA.Any) return PortableServer.ThreadPolicyValue;

   function To_Any
     (Item : PortableServer.ThreadPolicyValue) return CORBA.Any;
   function Wrap (X : access PortableServer.LifespanPolicyValue) return PolyORB.Any.Content'Class;

   TC_LifespanPolicyValue : CORBA.TypeCode.Object;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.LifespanPolicyValue;

   function From_Any (Item : CORBA.Any) return PortableServer.LifespanPolicyValue;

   function To_Any
     (Item : PortableServer.LifespanPolicyValue) return CORBA.Any;
   function Wrap (X : access PortableServer.IdUniquenessPolicyValue) return PolyORB.Any.Content'Class;

   TC_IdUniquenessPolicyValue : CORBA.TypeCode.Object;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.IdUniquenessPolicyValue;

   function From_Any (Item : CORBA.Any) return PortableServer.IdUniquenessPolicyValue;

   function To_Any
     (Item : PortableServer.IdUniquenessPolicyValue) return CORBA.Any;
   function Wrap (X : access PortableServer.IdAssignmentPolicyValue) return PolyORB.Any.Content'Class;

   TC_IdAssignmentPolicyValue : CORBA.TypeCode.Object;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.IdAssignmentPolicyValue;

   function From_Any (Item : CORBA.Any) return PortableServer.IdAssignmentPolicyValue;

   function To_Any
     (Item : PortableServer.IdAssignmentPolicyValue) return CORBA.Any;
   function Wrap (X : access PortableServer.ImplicitActivationPolicyValue) return PolyORB.Any.Content'Class;

   TC_ImplicitActivationPolicyValue : CORBA.TypeCode.Object;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.ImplicitActivationPolicyValue;

   function From_Any (Item : CORBA.Any) return PortableServer.ImplicitActivationPolicyValue;

   function To_Any
     (Item : PortableServer.ImplicitActivationPolicyValue) return CORBA.Any;
   function Wrap (X : access PortableServer.ServantRetentionPolicyValue) return PolyORB.Any.Content'Class;

   TC_ServantRetentionPolicyValue : CORBA.TypeCode.Object;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.ServantRetentionPolicyValue;

   function From_Any (Item : CORBA.Any) return PortableServer.ServantRetentionPolicyValue;

   function To_Any
     (Item : PortableServer.ServantRetentionPolicyValue) return CORBA.Any;
   function Wrap (X : access PortableServer.RequestProcessingPolicyValue) return PolyORB.Any.Content'Class;

   TC_RequestProcessingPolicyValue : CORBA.TypeCode.Object;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.RequestProcessingPolicyValue;

   function From_Any (Item : CORBA.Any) return PortableServer.RequestProcessingPolicyValue;

   function To_Any
     (Item : PortableServer.RequestProcessingPolicyValue) return CORBA.Any;

end PortableServer.Helper;
