------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 R T P O R T A B L E S E R V E R . P O A                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

--  $Id$

with PolyORB.POA;

package body RTPortableServer.POA is

   ------------
   -- To_Ref --
   ------------

   function To_Ref
     (Self : CORBA.Object.Ref'Class)
     return Ref
   is
      Result : Ref;
   begin
      if CORBA.Object.Entity_Of (Self).all
        not in PolyORB.POA.Obj_Adapter'Class then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      Set (Result, CORBA.Object.Entity_Of (Self));
      return Result;
   end To_Ref;

   ------------------------------------
   -- Create_Reference_With_Priority --
   ------------------------------------

   function Create_Reference_With_Priority
     (Self      : in Ref;
      Intf      : in CORBA.RepositoryId;
      Priority  : in RTCORBA.Priority)
     return CORBA.Object.Ref is
   begin
      pragma Warnings (Off);
      return Create_Reference_With_Priority (Self, Intf, Priority);
      pragma Warnings (On);
   end Create_Reference_With_Priority;

   -------------------------------------------
   -- Create_Reference_With_Id_And_Priority --
   -------------------------------------------

   function Create_Reference_With_Id_And_Priority
     (Self      : in Ref;
      Oid       : in PortableServer.ObjectId;
      Intf      : in CORBA.RepositoryId;
      Priority  : in RTCORBA.Priority)
     return CORBA.Object.Ref is
   begin
      pragma Warnings (Off);
      return Create_Reference_With_Id_And_Priority (Self, Oid, Intf, Priority);
      pragma Warnings (On);
   end Create_Reference_With_Id_And_Priority;

   -----------------------------------
   -- Activate_Object_With_Priority --
   -----------------------------------

   function Activate_Object_With_Priority
     (Self       : in Ref;
      P_Servant  : in PortableServer.Servant;
      Priority   : in RTCORBA.Priority)
     return PortableServer.ObjectId is
   begin
      pragma Warnings (Off);
      return Activate_Object_With_Priority (Self, P_Servant, Priority);
      pragma Warnings (On);
   end Activate_Object_With_Priority;

   ------------------------------------------
   -- Activate_Object_With_Id_And_Priority --
   ------------------------------------------

   procedure Activate_Object_With_Id_And_Priority
     (Self      : in Ref;
      Oid       : in PortableServer.ObjectId;
      P_Servant : in PortableServer.Servant;
      Priority  : in RTCORBA.Priority)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, Oid, P_Servant, Priority);
      pragma Warnings (On);
   begin
      null;
   end Activate_Object_With_Id_And_Priority;

end RTPortableServer.POA;
