------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             C O R B A . D O M A I N M A N A G E R . S K E L              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2008, Free Software Foundation, Inc.          --
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
with CORBA;
pragma Elaborate_All (CORBA);
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

   Is_A_Arg_Name_�_Type_Id : constant CORBA.Identifier
   := CORBA.To_CORBA_String ("Type_Id");

   get_domain_policy_Arg_Name_�_policy_type : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("policy_type");

   procedure Invoke
     (Self : PortableServer.Servant;
      Request : CORBA.ServerRequest.Object_Ptr)
   is
      Operation : constant PolyORB.Std.String
         := CORBA.To_Standard_String
              (CORBA.ServerRequest.Operation
               (Request.all));
      Arg_List_� : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List (0, Arg_List_�);
      if Operation = "_is_a" then
         declare
            Type_Id : CORBA.String;
            Arg_Any_�_Type_Id : constant CORBA.Any := CORBA.To_Any (Type_Id);

            Result_� : CORBA.Boolean;
         begin
            CORBA.NVList.Add_Item
            (Arg_List_�,
            Is_A_Arg_Name_�_Type_Id,
            Arg_Any_�_Type_Id,
            CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_�);

            begin
               --  Convert arguments from their Any

               Type_Id :=
                 CORBA.From_Any (Arg_Any_�_Type_Id);

               --  Call implementation

               Result_� := CORBA.DomainManager.Is_A
                 (CORBA.To_Standard_String (Type_Id));
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
            (Request,
            CORBA.To_Any (Result_�));
         end;

      elsif Operation = "_non_existent"
        or else Operation = "_not_existent"
      then

         CORBA.ServerRequest.Arguments (Request, Arg_List_�);

         CORBA.ServerRequest.Set_Result
           (Request,
            CORBA.To_Any (CORBA.Boolean'(False)));

      elsif Operation = "_interface" then

         CORBA.ServerRequest.Arguments (Request, Arg_List_�);

         CORBA.ServerRequest.Set_Result
           (Request,
            CORBA.Object.Helper.To_Any
            (CORBA.Object.Ref
             (PolyORB.CORBA_P.IR_Hooks.Get_Interface_Definition
              (CORBA.To_CORBA_String (Repository_Id)))));

      elsif Operation = "_domain_managers" then

         CORBA.ServerRequest.Arguments (Request, Arg_List_�);

         CORBA.ServerRequest.Set_Result
           (Request,
            PolyORB.CORBA_P.Domain_Management.Get_Domain_Managers
            (Self));

      elsif Operation = "get_domain_policy" then

         declare
            Argument_�_policy_type : CORBA.PolicyType;
            pragma Warnings (Off, Argument_�_policy_type);
            Arg_CC_�_policy_type  : aliased PolyORB.Any.Content'Class :=
               CORBA.Wrap (CORBA.Unsigned_Long (Argument_�_policy_type)'Unrestricted_Access);
            Arg_Any_�_policy_type : constant CORBA.Any := CORBA.Internals.Get_Wrapper_Any (CORBA.Helper.TC_PolicyType, Arg_CC_�_policy_type'Unchecked_Access);

            Result_�              : CORBA.Policy.Ref;
            pragma Warnings (Off, Result_�);
            Arg_CC_�_Result_�     : aliased PolyORB.Any.Content'Class :=
               CORBA.Object.Helper.Wrap (CORBA.Object.Ref (Result_�)'Unrestricted_Access);
            Arg_Any_�_Result_�    : constant CORBA.Any := CORBA.Internals.Get_Wrapper_Any (CORBA.Policy.Helper.TC_Policy, Arg_CC_�_Result_�'Unchecked_Access);
         begin
            CORBA.NVList.Add_Item
              (Arg_List_�,
               get_domain_policy_Arg_Name_�_policy_type,
               Arg_Any_�_policy_type,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_�);

            begin
               Result_� := CORBA.DomainManager.Impl.get_domain_policy
                 (CORBA.DomainManager.Impl.Object'Class (Self.all)'Access,
                  Argument_�_policy_type);
            end;
            CORBA.ServerRequest.Set_Result
              (Request, Arg_Any_�_Result_�);
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
           (Request, E);
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
