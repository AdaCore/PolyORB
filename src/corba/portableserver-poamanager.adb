------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O R T A B L E S E R V E R . P O A M A N A G E R             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

--  $Id: //droopi/main/src/corba/portableserver-poamanager.adb#12 $

with PolyORB.Exceptions;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

with PolyORB.CORBA_P.Exceptions;

package body PortableServer.POAManager is

   use PolyORB.Exceptions;
   use PolyORB.POA_Manager;

   function To_POA_Manager (Self : Ref) return POAManager_Access;
   --  Convert a Ref to the designated POAManager_Access.  Check the
   --  reference points to a non null POAM, the type of the referenced
   --  object (else BAD_PARAM is raised).  Check that the POAM is
   --  active (else AdapterInactive is raised).

   function To_Active_POA_Manager (Self : Ref) return POAManager_Access;
   --  Same as above spec. Ensure that the designated POAM is active.

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterInactive_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= AdapterInactive'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := AdapterInactive_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   ---------------------------
   -- Raise_AdapterInactive --
   ---------------------------

   procedure Raise_AdapterInactive
     (Excp_Memb : in AdapterInactive_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
   begin
      raise AdapterInactive;
   end Raise_AdapterInactive;

   ----------------------
   -- Raise_From_Error --
   ----------------------

   procedure Raise_From_Error
     (Error : in out PolyORB.Exceptions.Error_Container) is

   begin
      pragma Assert (Is_Error (Error));

      case Error.Kind is
         when AdapterInactive_E =>
            declare
               Member : constant AdapterInactive_Members
                 := AdapterInactive_Members (Error.Member.all);
            begin
               Free (Error.Member);
               Raise_AdapterInactive (Member);
            end;

         when others =>
            raise Program_Error;
      end case;
   end Raise_From_Error;

   --------------------
   -- To_POA_Manager --
   --------------------

   function To_POA_Manager (Self : Ref)
     return POAManager_Access
   is
      Res : constant PolyORB.Smart_Pointers.Entity_Ptr := Entity_Of (Self);

   begin
      if Is_Nil (Self)
        or else Res.all not in PolyORB.POA_Manager.POAManager'Class then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      return POAManager_Access (Res);
   end To_POA_Manager;

   ---------------------------
   -- To_Active_POA_Manager --
   ---------------------------

   function To_Active_POA_Manager (Self : Ref)
                                  return POAManager_Access
   is
      Res : constant POAManager_Access := To_POA_Manager (Self);
   begin
      if Get_State (Res.all) = INACTIVE then
         raise AdapterInactive;
      end if;

      return Res;
   end To_Active_POA_Manager;

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Self : in Ref)
   is
      POA_Manager : constant POAManager_Access
        := To_Active_POA_Manager (Self);

      Error : Error_Container;

   begin
      Activate (POA_Manager, Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Activate;

   -------------------
   -- Hold_Requests --
   -------------------

   procedure Hold_Requests
     (Self                : in Ref;
      Wait_For_Completion : in CORBA.Boolean)
   is
      POA_Manager : constant POAManager_Access
        := To_Active_POA_Manager (Self);

      Error : Error_Container;

   begin
      Hold_Requests (POA_Manager, Wait_For_Completion, Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Hold_Requests;

   ----------------------
   -- Discard_Requests --
   ----------------------

   procedure Discard_Requests
     (Self                : in Ref;
      Wait_For_Completion : in CORBA.Boolean)
   is
      POA_Manager : constant POAManager_Access
        := To_Active_POA_Manager (Self);

      Error : Error_Container;

   begin
      Discard_Requests (POA_Manager, Wait_For_Completion, Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Discard_Requests;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate
     (Self                : in Ref;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean)
   is
      POA_Manager : constant POAManager_Access
        := To_Active_POA_Manager (Self);

   begin
      Deactivate (POA_Manager, Etherealize_Objects, Wait_For_Completion);
   end Deactivate;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
     (Self : in Ref)
     return State
   is
      POA_Manager : constant POAManager_Access
        := To_POA_Manager (Self);

   begin
      return Get_State (POA_Manager.all);
   end Get_State;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.CORBA_P.Exceptions.POAManager_Raise_From_Error
        := Raise_From_Error'Access;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"portableserver.poamanager",
       Conflicts => Empty,
       Depends => Empty,
       Provides => Empty,
       Init => Initialize'Access));

end PortableServer.POAManager;
