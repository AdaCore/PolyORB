------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O R T A B L E S E R V E R . P O A . H E L P E R             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC version 2.3.0w.
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks ("NM32766");

with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;

package PortableServer.POA.Helper is

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class) return PortableServer.POA.Local_Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class) return PortableServer.POA.Local_Ref;

   TC_POA : CORBA.TypeCode.Object;

   TC_AdapterAlreadyExists : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return PortableServer.POA.AdapterAlreadyExists_Members;

   function To_Any
     (Item : PortableServer.POA.AdapterAlreadyExists_Members) return CORBA.Any;

   procedure Raise_AdapterAlreadyExists
     (Members : AdapterAlreadyExists_Members);
   pragma No_Return (Raise_AdapterAlreadyExists);

   TC_AdapterNonExistent : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return PortableServer.POA.AdapterNonExistent_Members;

   function To_Any
     (Item : PortableServer.POA.AdapterNonExistent_Members) return CORBA.Any;

   procedure Raise_AdapterNonExistent
     (Members : AdapterNonExistent_Members);
   pragma No_Return (Raise_AdapterNonExistent);

   TC_InvalidPolicy : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return PortableServer.POA.InvalidPolicy_Members;

   function To_Any
     (Item : PortableServer.POA.InvalidPolicy_Members) return CORBA.Any;

   procedure Raise_InvalidPolicy
     (Members : InvalidPolicy_Members);
   pragma No_Return (Raise_InvalidPolicy);

   TC_NoServant : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return PortableServer.POA.NoServant_Members;

   function To_Any
     (Item : PortableServer.POA.NoServant_Members) return CORBA.Any;

   procedure Raise_NoServant
     (Members : NoServant_Members);
   pragma No_Return (Raise_NoServant);

   TC_ObjectAlreadyActive : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return PortableServer.POA.ObjectAlreadyActive_Members;

   function To_Any
     (Item : PortableServer.POA.ObjectAlreadyActive_Members) return CORBA.Any;

   procedure Raise_ObjectAlreadyActive
     (Members : in ObjectAlreadyActive_Members);
   pragma No_Return (Raise_ObjectAlreadyActive);

   TC_ObjectNotActive : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return PortableServer.POA.ObjectNotActive_Members;

   function To_Any
     (Item : PortableServer.POA.ObjectNotActive_Members) return CORBA.Any;

   procedure Raise_ObjectNotActive
     (Members : in ObjectNotActive_Members);
   pragma No_Return (Raise_ObjectNotActive);

   TC_ServantAlreadyActive : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return PortableServer.POA.ServantAlreadyActive_Members;

   function To_Any
     (Item : PortableServer.POA.ServantAlreadyActive_Members) return CORBA.Any;

   procedure Raise_ServantAlreadyActive
     (Members : ServantAlreadyActive_Members);
   pragma No_Return (Raise_ServantAlreadyActive);

   TC_ServantNotActive : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return PortableServer.POA.ServantNotActive_Members;

   function To_Any
     (Item : PortableServer.POA.ServantNotActive_Members) return CORBA.Any;

   procedure Raise_ServantNotActive
     (Members : ServantNotActive_Members);
   pragma No_Return (Raise_ServantNotActive);

   TC_WrongAdapter : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return PortableServer.POA.WrongAdapter_Members;

   function To_Any
     (Item : PortableServer.POA.WrongAdapter_Members) return CORBA.Any;

   procedure Raise_WrongAdapter
     (Members : WrongAdapter_Members);
   pragma No_Return (Raise_WrongAdapter);

   TC_WrongPolicy : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return PortableServer.POA.WrongPolicy_Members;

   function To_Any
     (Item : PortableServer.POA.WrongPolicy_Members) return CORBA.Any;

   procedure Raise_WrongPolicy
     (Members : WrongPolicy_Members);
   pragma No_Return (Raise_WrongPolicy);

end PortableServer.POA.Helper;
