------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O R T A B L E S E R V E R . P O A M A N A G E R             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;

with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

with PolyORB.CORBA_P.Exceptions;

package body PortableServer.POAManager is

   use PolyORB.Errors;
   use PolyORB.POA_Manager;

   function To_POA_Manager (Self : Local_Ref) return POAManager_Access;
   --  Convert a Local_Ref to the designated POAManager_Access.  Check the
   --  reference points to a non null POAM, the type of the referenced
   --  object (else BAD_PARAM is raised).  Check that the POAM is
   --  active (else AdapterInactive is raised).

   --------------------
   -- To_POA_Manager --
   --------------------

   function To_POA_Manager
     (Self : Local_Ref)
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

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Self : Local_Ref)
   is
      POA_Manager : constant POAManager_Access := To_POA_Manager (Self);
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
     (Self                : Local_Ref;
      Wait_For_Completion : CORBA.Boolean)
   is
      POA_Manager : constant POAManager_Access := To_POA_Manager (Self);
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
     (Self                : Local_Ref;
      Wait_For_Completion : CORBA.Boolean)
   is
      POA_Manager : constant POAManager_Access := To_POA_Manager (Self);
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
     (Self                : Local_Ref;
      Etherealize_Objects : CORBA.Boolean;
      Wait_For_Completion : CORBA.Boolean)
   is
      POA_Manager : constant POAManager_Access := To_POA_Manager (Self);

   begin
      Deactivate (POA_Manager, Etherealize_Objects, Wait_For_Completion);
   end Deactivate;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
     (Self : Local_Ref)
     return State
   is
      POA_Manager : constant POAManager_Access := To_POA_Manager (Self);

      State : constant PolyORB.POA_Manager.State
        := Get_State (POA_Manager.all);
   begin
      return PortableServer.POAManager.State (State);
   end Get_State;

   ----------------------
   -- Raise_From_Error --
   ----------------------

   procedure Raise_From_Error
     (Error   : in out PolyORB.Errors.Error_Container;
      Message : Standard.String)
   is
   begin
      pragma Assert (Is_Error (Error));

      case Error.Kind is
         when AdapterInactive_E =>
            declare
               Member : constant AdapterInactive_Members
                 := AdapterInactive_Members'(CORBA.IDL_Exception_Members
                                             with null record);
            begin
               Free (Error.Member);
               Raise_AdapterInactive (Member, Message);
            end;

         when others =>
            raise Program_Error;
      end case;
   end Raise_From_Error;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
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
     (Excp_Memb : AdapterInactive_Members;
      Message   : Standard.String := "")
   is
      pragma Unreferenced (Excp_Memb);

   begin
      Ada.Exceptions.Raise_Exception (AdapterInactive'Identity, Message);
   end Raise_AdapterInactive;

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
      (Name      => +"portableserver.poamanager",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PortableServer.POAManager;
