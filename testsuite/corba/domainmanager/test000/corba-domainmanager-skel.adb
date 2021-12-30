------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             C O R B A . D O M A I N M A N A G E R . S K E L              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2021, Free Software Foundation, Inc.          --
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

pragma Style_Checks ("NM32766");

with PolyORB.Utils.Strings;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);
with PolyORB.QoS.Exception_Informations;
with CORBA.Policy.Helper;
with CORBA.Policy;
with CORBA.Helper;
with PolyORB.Any;
with PolyORB.CORBA_P.Domain_Management;
with PolyORB.CORBA_P.IR_Hooks;
with CORBA.Object.Helper;
with CORBA.ORB;
with CORBA.NVList;
with PolyORB.Std;
with CORBA.ServerRequest;
with CORBA.DomainManager.Impl;
with PortableServer;
pragma Elaborate_All (PortableServer);
with PolyORB.CORBA_P.Exceptions;

package body CORBA.DomainManager.Skel is

   --  Skeleton subprograms

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean;

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean is
   begin
      return Obj.all in CORBA.DomainManager.Impl.Object'Class;
   end Servant_Is_A;

   Is_A_Arg_Name_Ü_Type_Id : constant CORBA.Identifier
   := CORBA.To_CORBA_String ("Type_Id");

   get_domain_policy_Arg_Name_Ü_policy_type : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("policy_type");

   procedure Invoke
     (Self : PortableServer.Servant;
      Request : CORBA.ServerRequest.Object_Ptr)
   is
      Operation : constant PolyORB.Std.String
         := CORBA.To_Standard_String
              (CORBA.ServerRequest.Operation
               (Request.all));
      Arg_List_Ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List (0, Arg_List_Ü);
      if Operation = "_is_a" then
         declare
            Type_Id : CORBA.String;
            Arg_Any_Ü_Type_Id : constant CORBA.Any := CORBA.To_Any (Type_Id);

            Result_Ü : CORBA.Boolean;
         begin
            CORBA.NVList.Add_Item
            (Arg_List_Ü,
            Is_A_Arg_Name_Ü_Type_Id,
            Arg_Any_Ü_Type_Id,
            CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               Type_Id :=
                 CORBA.From_Any (Arg_Any_Ü_Type_Id);

               --  Call implementation

               Result_Ü := CORBA.DomainManager.Is_A
                 (CORBA.To_Standard_String (Type_Id));
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
            (Request,
            CORBA.To_Any (Result_Ü));
         end;

      elsif Operation = "_non_existent"
        or else Operation = "_not_existent"
      then

         CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

         CORBA.ServerRequest.Set_Result
           (Request,
            CORBA.To_Any (CORBA.Boolean'(False)));

      elsif Operation = "_interface" then

         CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

         CORBA.ServerRequest.Set_Result
           (Request,
            CORBA.Object.Helper.To_Any
            (CORBA.Object.Ref
             (PolyORB.CORBA_P.IR_Hooks.Get_Interface_Definition
              (CORBA.To_CORBA_String (Repository_Id)))));

      elsif Operation = "_domain_managers" then

         CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

         CORBA.ServerRequest.Set_Result
           (Request,
            PolyORB.CORBA_P.Domain_Management.Get_Domain_Managers
            (Self));

      elsif Operation = "get_domain_policy" then

         declare
            Argument_Ü_policy_type : CORBA.PolicyType;
            pragma Warnings (Off, Argument_Ü_policy_type);
            Arg_CC_Ü_policy_type  : aliased PolyORB.Any.Content'Class :=
               CORBA.Wrap (CORBA.Unsigned_Long (Argument_Ü_policy_type)'Unrestricted_Access);
            Arg_Any_Ü_policy_type : constant CORBA.Any := CORBA.Internals.Get_Wrapper_Any (CORBA.Helper.TC_PolicyType, Arg_CC_Ü_policy_type'Unchecked_Access);

            Result_Ü              : CORBA.Policy.Ref;
            pragma Warnings (Off, Result_Ü);
            Arg_CC_Ü_Result_Ü     : aliased PolyORB.Any.Content'Class :=
               CORBA.Object.Helper.Wrap (CORBA.Object.Ref (Result_Ü)'Unrestricted_Access);
            Arg_Any_Ü_Result_Ü    : constant CORBA.Any := CORBA.Internals.Get_Wrapper_Any (CORBA.Policy.Helper.TC_Policy, Arg_CC_Ü_Result_Ü'Unchecked_Access);
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               get_domain_policy_Arg_Name_Ü_policy_type,
               Arg_Any_Ü_policy_type,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               Result_Ü := CORBA.DomainManager.Impl.get_domain_policy
                 (CORBA.DomainManager.Impl.Object'Class (Self.all)'Access,
                  Argument_Ü_policy_type);
            end;
            CORBA.ServerRequest.Set_Result
              (Request, Arg_Any_Ü_Result_Ü);
            return;
         end;

      else
         CORBA.Raise_Bad_Operation (CORBA.Default_Sys_Member);
      end if;
   exception
      when E : others =>
         CORBA.ServerRequest.Set_Exception
           (Request,
            PolyORB.CORBA_P.Exceptions.System_Exception_To_Any (E));
         PolyORB.QoS.Exception_Informations.Set_Exception_Information
           (Request.all, E);
   end Invoke;

   procedure Deferred_Initialization is
   begin
      PortableServer.Internals.Register_Skeleton
        (CORBA.DomainManager.Repository_Id,
         Servant_Is_A'Access,
         Is_A'Access,
         Invoke'Access);

   end Deferred_Initialization;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"CORBA.DomainManager.Skel",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   =>
                  PolyORB.Initialization.String_Lists.Empty
          ,
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access,
          Shutdown  => null));
   end;

end CORBA.DomainManager.Skel;
