------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O R T A B L E S E R V E R . P O A M A N A G E R             --
--                                                                          --
--                                 B o d y                                  --
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

--  $Id: //droopi/main/src/corba/portableserver-poamanager.adb#3 $

with PolyORB.CORBA_P.Exceptions;
with PolyORB.POA_Manager; use PolyORB.POA_Manager;
with PolyORB.Smart_Pointers;

package body PortableServer.POAManager is

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterInactive_Members)
   is
      use Ada.Exceptions;
   begin
      if Exception_Identity (From) /= AdapterInactive'Identity then
         PolyORB.CORBA_P.Exceptions.Raise_Bad_Param;
      end if;
      To := AdapterInactive_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   function To_Active_POA_Manager
     (Self : Ref)
     return POAManager_Object_Ptr;
   --  Convert a Ref to the designated POAManager_Object_Ptr.
   --  Check the type of the referenced object (else BAD_PARAM is raised).
   --  Check that the POAM is active (else AdapterInactive is raised).

   function To_Active_POA_Manager
     (Self : Ref)
     return POAManager_Object_Ptr
   is
      Res : constant PolyORB.Smart_Pointers.Entity_Ptr := Entity_Of (Self);
   begin
      if Is_Nil (Self)
        or else Res.all not in PolyORB.POA_Manager.POAManager'Class then
         PolyORB.CORBA_P.Exceptions.Raise_Bad_Param;
      end if;

      if Get_State (POAManager_Object_Ptr (Res).all) = INACTIVE then
         raise AdapterInactive;
      end if;

      return POAManager_Object_Ptr (Res);
   end To_Active_POA_Manager;

   procedure Activate (Self : Ref) is
      POA_Manager : constant POAManager_Object_Ptr
        := To_Active_POA_Manager (Self);
   begin
      Activate (POA_Manager);
   end Activate;

   procedure Hold_Requests (Self : Ref; Wait_For_Completion : CORBA.Boolean)
   is
      POA_Manager : constant POAManager_Object_Ptr
        := To_Active_POA_Manager (Self);
   begin
      Hold_Requests (POA_Manager, Wait_For_Completion);
   end Hold_Requests;

   procedure Discard_Requests (Self : Ref; Wait_For_Completion : CORBA.Boolean)
   is
      POA_Manager : constant POAManager_Object_Ptr
        := To_Active_POA_Manager (Self);
   begin
      Discard_Requests (POA_Manager, Wait_For_Completion);
   end Discard_Requests;

   procedure Deactivate
     (Self : in Ref;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean)
   is
      POA_Manager : constant POAManager_Object_Ptr
        := To_Active_POA_Manager (Self);
   begin
      Deactivate (POA_Manager, Etherealize_Objects, Wait_For_Completion);
   end Deactivate;

end PortableServer.POAManager;
