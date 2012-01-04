------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    T E S T 0 0 1 _ C L I E N T _ R E Q U E S T _ I N F O _ T E S T S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with CORBA.Object;
with CORBA.Policy;
with IOP;

with Test001_Interface.Helper;

package body Test001_Client_Request_Info_Tests is

   use CORBA;
   use CORBA.Object;
   use CORBA.TypeCode;
   use PortableInterceptor.ClientRequestInfo;
   use Test001_Globals;
   use Test001_Interface;
   use Test001_Interface.Helper;

   --------------------------------------
   -- Test_Add_Request_Service_Context --
   --------------------------------------

   procedure Test_Add_Request_Service_Context
     (Point : Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Operation : constant String  := "add_request_service_context";
      Valid     : constant Boolean := Point = Send_Request;

   begin
      Add_Request_Service_Context (Info, Test_Request_Context, False);
      if Valid then
         Output (Point, Operation, True);
      else
         Output (Point, Operation, False);
      end if;

   exception
      when E : Bad_Inv_Order =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if not Valid and then Members.Minor = OMGVMCID + 14 then
               Output (Point, Operation, True);
            else
               Output (Point, Operation, False);
            end if;
         end;

      when others =>
         Output (Point, Operation, False);
   end Test_Add_Request_Service_Context;

   ----------------------------
   -- Test_Effective_Profile --
   ----------------------------

   procedure Test_Effective_Profile
     (Point : Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      use type IOP.ProfileId;

      Operation : constant String := "effective_profile";

      Profile   : IOP.TaggedProfile;

   begin
      Profile := Get_Effective_Profile (Info);

      if Profile.Tag /= IOP.Tag_Internet_IOP then
         Output (Point, Operation, False);

      else
         Output (Point, Operation, True);
      end if;

   exception
      when others =>
         Output (Point, Operation, False);
   end Test_Effective_Profile;

   ---------------------------
   -- Test_Effective_Target --
   ---------------------------

   procedure Test_Effective_Target
     (Point : Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Operation : constant String := "effective_target";
      Obj       : CORBA.Object.Ref;

   begin
      Obj := Get_Effective_Target (Info);

      if Is_Equivalent (Obj, Test001_Globals.Test_Object) then
         Output (Point, Operation, True);
      else
         Output (Point, Operation, False);
      end if;

   exception
      when others =>
         Output (Point, Operation, False);
   end Test_Effective_Target;

   ----------------------------------
   -- Test_Get_Effective_Component --
   ----------------------------------

   procedure Test_Get_Effective_Component
     (Point : Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      use type IOP.ComponentId;

      Operation : constant String := "get_effective_component";
      Valid     : constant Boolean := Point /= Send_Poll;
      Aux       : IOP.TaggedComponent;

   begin
      begin
         Aux := Get_Effective_Component (Info, IOP.Tag_Code_Sets);

         if not Valid then
            Output (Point, Operation, False);
            return;

         elsif Aux.Tag /= IOP.Tag_Code_Sets then
            Output (Point, Operation, False);
            return;
         end if;

      exception
         when E : Bad_Inv_Order =>
            declare
               Members : System_Exception_Members;
            begin
               Get_Members (E, Members);
               if Valid or else Members.Minor /= OMGVMCID + 14 then
                  Output (Point, Operation, False);
                  return;
               end if;
            end;

         when others =>
            Output (Point, Operation, False);
            return;
      end;

      begin
         Aux := Get_Effective_Component (Info, IOP.Tag_Null_Tag);

         Output (Point, Operation, False);

      exception
         when E : Bad_Param =>
            declare
               Members : System_Exception_Members;
            begin
               Get_Members (E, Members);
               if Members.Minor = OMGVMCID + 28 then
                  Output (Point, Operation, True);
               else
                  Output (Point, Operation, False);
               end if;
            end;

         when others =>
            Output (Point, Operation, False);
      end;
   end Test_Get_Effective_Component;

   -----------------------------------
   -- Test_Get_Effective_Components --
   -----------------------------------

   procedure Test_Get_Effective_Components
     (Point : Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      use type IOP.ComponentId;

      Operation : constant String := "get_effective_components";
      Valid     : constant Boolean := Point /= Send_Poll;
      Aux       : IOP.TaggedComponentSeq;

   begin
      begin
         Aux := Get_Effective_Components (Info, IOP.Tag_Code_Sets);

         if not Valid then
            Output (Point, Operation, False);
            return;

         elsif IOP.Length (Aux) /= 1 then
            Output (Point, Operation, False);
            return;

         elsif IOP.Get_Element (Aux, 1).Tag /= IOP.Tag_Code_Sets then
            Output (Point, Operation, False);
            return;
         end if;

      exception
         when E : Bad_Inv_Order =>
            declare
               Members : System_Exception_Members;
            begin
               Get_Members (E, Members);
               if Valid or else Members.Minor /= OMGVMCID + 14 then
                  Output (Point, Operation, False);
                  return;
               end if;
            end;

         when others =>
            Output (Point, Operation, False);
            return;
      end;

      begin
         Aux := Get_Effective_Components (Info, IOP.Tag_Null_Tag);

         Output (Point, Operation, False);

      exception
         when E : Bad_Param =>
            declare
               Members : System_Exception_Members;
            begin
               Get_Members (E, Members);
               if Members.Minor = OMGVMCID + 28 then
                  Output (Point, Operation, True);
               else
                  Output (Point, Operation, False);
               end if;
            end;

         when others =>
            Output (Point, Operation, False);
      end;
   end Test_Get_Effective_Components;

   -----------------------------
   -- Test_Get_Request_Policy --
   -----------------------------

   procedure Test_Get_Request_Policy
     (Point : Client_Interception_Point;
      Info  : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Operation : constant String := "get_request_policy";
      Valid     : constant Boolean := Point /= Send_Poll;
      Pol       : CORBA.Policy.Ref;
      --  pragma Unreferenced (Pol);
      pragma Warnings (Off, Pol); --  WAG:5.02 DB08-008
      --  Assigned but never read

   begin
      --  XXX Functionality test not implemented

      Pol := Get_Request_Policy (Info, 1);

      if not Valid then
         Output (Point, Operation, False);
      else
         Output (Point, Operation, True);
      end if;

   exception
      when E : Inv_Policy =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if Valid and then Members.Minor = OMGVMCID + 2 then
               Output (Point, Operation, True, " (INV_POLICY)");
            else
               Output (Point, Operation, False);
            end if;
         end;

      when E : Bad_Inv_Order =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if not Valid and then Members.Minor = OMGVMCID + 14 then
               Output (Point, Operation, True);
            else
               Output (Point, Operation, False);
            end if;
         end;

      when others =>
         Output (Point, Operation, False);
   end Test_Get_Request_Policy;

   -----------------------------
   -- Test_Received_Exception --
   -----------------------------

   procedure Test_Received_Exception
     (Point  : Client_Interception_Point;
      Info   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Operation : constant String := "received_exception";
      Valid     : constant Boolean := Point = Receive_Exception;
      Exc       : Any;

   begin
      Exc := Get_Received_Exception (Info);

      if not Valid then
         Output (Point, Operation, False);
      elsif Get_Type (Exc) /= TC_Test_Exception then
         Output (Point, Operation, False);
      else
         Output (Point, Operation, True);
      end if;

   exception
      when E : Bad_Inv_Order =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if not Valid and then Members.Minor = OMGVMCID + 14 then
               Output (Point, Operation, True);
            else
               Output (Point, Operation, False);
            end if;
         end;

      when others =>
         Output (Point, Operation, False);
   end Test_Received_Exception;

   --------------------------------
   -- Test_Received_Exception_Id --
   --------------------------------

   procedure Test_Received_Exception_Id
     (Point  : Client_Interception_Point;
      Info   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Operation : constant String := "received_exception_id";
      Valid     : constant Boolean := Point = Receive_Exception;
      Id        : RepositoryId;

   begin
      Id := Get_Received_Exception_Id (Info);

      if Id /= Test_Exception_Repository_Id then
         Output (Point, Operation, False);
      else
         Output (Point, Operation, True);
      end if;

   exception
      when E : Bad_Inv_Order =>
         declare
            Members : System_Exception_Members;
         begin
            Get_Members (E, Members);
            if not Valid and then Members.Minor = OMGVMCID + 14 then
               Output (Point, Operation, True);
            else
               Output (Point, Operation, False);
            end if;
         end;

      when others =>
         Output (Point, Operation, False);
   end Test_Received_Exception_Id;

   -----------------
   -- Test_Target --
   -----------------

   procedure Test_Target
     (Point  : Client_Interception_Point;
      Info   : PortableInterceptor.ClientRequestInfo.Local_Ref)
   is
      Operation : constant String := "target";
      Obj       : CORBA.Object.Ref;

   begin
      Obj := Get_Target (Info);

      if Is_Equivalent (Obj, Test001_Globals.Test_Object) then
         Output (Point, Operation, True);
      else
         Output (Point, Operation, False);
      end if;

   exception
      when others =>
         Output (Point, Operation, False);
   end Test_Target;

end Test001_Client_Request_Info_Tests;
