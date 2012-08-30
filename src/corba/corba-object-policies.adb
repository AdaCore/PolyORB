------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                C O R B A . O B J E C T . P O L I C I E S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Annotations;
with PolyORB.Any.NVList;
with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.ORB;
with PolyORB.References.Binding;
with PolyORB.Requests;
with PolyORB.Setup;
with PolyORB.Tasking.Threads.Annotations;

with CORBA.DomainManager.Helper;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Local;
with PolyORB.CORBA_P.Policy_Management;

package body CORBA.Object.Policies is

   use PolyORB.Annotations;
   use PolyORB.CORBA_P.Policy_Management;

   -----------------------
   -- Get_Client_Policy --
   -----------------------

   function Get_Client_Policy
     (Self     : Ref'Class;
      The_Type : PolicyType)
      return CORBA.Policy.Ref
   is
      Npad   : Notepad_Access;
      Note   : Policy_Manager_Note;
      Result : CORBA.Policy.Ref;

   begin
      if Is_Nil (Self) then
         Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      if PolyORB.CORBA_P.Local.Is_Local (Self) then
         Raise_No_Implement (No_Implement_Members'(Minor     => 3,
                                                   Completed => Completed_No));
      end if;

      --  First, checking reference overrides

      Npad :=
        PolyORB.References.Notepad_Of (Internals.To_PolyORB_Ref (Ref (Self)));
      Get_Note (Npad.all, Note, Empty_Policy_Manager_Note);
      Result := Note.Overrides (The_Type);

      if not CORBA.Policy.Is_Null (Result) then
         return Result;
      end if;

      --  Second, checking thread overrides

      Npad := PolyORB.Tasking.Threads.Annotations.Get_Current_Thread_Notepad;
      Get_Note (Npad.all, Note, Empty_Policy_Manager_Note);
      Result := Note.Overrides (The_Type);

      if not CORBA.Policy.Is_Null (Result) then
         return Result;
      end if;

      --  Third, checking ORB overrides

      Npad := PolyORB.ORB.Notepad_Of (PolyORB.Setup.The_ORB);
      Get_Note (Npad.all, Note, Empty_Policy_Manager_Note);
      Result := Note.Overrides (The_Type);

      if not CORBA.Policy.Is_Null (Result) then
         return Result;
      end if;

      --  Last, try to find default value

      return Policy_System_Default_Value (The_Type);
   end Get_Client_Policy;

   -------------------------
   -- Get_Domain_Managers --
   -------------------------

   function Get_Domain_Managers
     (Self : Ref'Class)
      return CORBA.DomainManager.DomainManagersList
   is
      Operation_Name : constant Standard.String := "_domain_managers";

      Request     : aliased PolyORB.Requests.Request;
      Arg_List    : PolyORB.Any.NVList.Ref;
      Result      : PolyORB.Any.NamedValue;
      Result_Name : constant CORBA.String := To_CORBA_String ("Result");

   begin
      if CORBA.Object.Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      PolyORB.Any.NVList.Create (Arg_List);

      Result :=
        (Name      => PolyORB.Types.Identifier (Result_Name),
         Argument  => CORBA.Internals.Get_Empty_Any
          (CORBA.DomainManager.Helper.TC_IDL_SEQUENCE_DomainManager),
         Arg_Modes => 0);

      PolyORB.Requests.Setup_Request
        (Req       => Request,
         Target    => Internals.To_PolyORB_Ref (Ref (Self)),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request'Access, PolyORB.Requests.Flags (0));

      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence (Request);
      return CORBA.DomainManager.Helper.From_Any (CORBA.Any (Result.Argument));
   end Get_Domain_Managers;

   ----------------
   -- Get_Policy --
   ----------------

   function Get_Policy
     (Self        : Ref;
      Policy_Type : PolicyType)
      return CORBA.Policy.Ref
   is
      Result : CORBA.Policy.Ref;

   begin
      Result := Get_Client_Policy (Self, Policy_Type);

      if not Policy.Is_Nil (Result) then
         --  XXX Client policy should be reconcilied with value
         --  defined in IOR. Not supported for now.

         return Result;
      end if;

      if Is_Domain_Policy (Policy_Type) then
         --  Obtain domain list from Object

         declare
            use CORBA.DomainManager;
            use CORBA.DomainManager.IDL_SEQUENCE_DomainManager;

            Managers : constant DomainManagersList :=
              Get_Domain_Managers (Self);

         begin
            --  XXX For now we simply find the first domain manager which hold
            --  information about the requested policy and return policy value.
            --  This is not conformant with CORBA specifications which require
            --  to resolve policy overlapping conflicts but not define any way
            --  to do this (CORBA 3.0.3 par. 4.10.1.4 Object Membership of
            --  Policy Domains).

            for J in 1 .. Length (Managers) loop
               begin
                  Result :=
                    Get_Domain_Policy (Get_Element (Managers, J), Policy_Type);

                  if not Policy.Is_Nil (Result) then
                     return Result;
                  end if;

               exception
                  when CORBA.Inv_Policy =>
                     null;
               end;
            end loop;
         end;
      end if;

      --  There is no policy value found, raise CORBA::INV_POLICY exception.

      Raise_Inv_Policy (Default_Sys_Member);
   end Get_Policy;

   --------------------------
   -- Get_Policy_Overrides --
   --------------------------

   function Get_Policy_Overrides
     (Self  : Ref'Class;
      Types : CORBA.Policy.PolicyTypeSeq)
      return CORBA.Policy.PolicyList
   is
      Npad : Notepad_Access;
      Note : Policy_Manager_Note;

   begin
      if Is_Nil (Self) then
         Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      if PolyORB.CORBA_P.Local.Is_Local (Self) then
         Raise_No_Implement (No_Implement_Members'(Minor     => 3,
                                                   Completed => Completed_No));
      end if;

      Npad :=
        PolyORB.References.Notepad_Of (Internals.To_PolyORB_Ref (Ref (Self)));
      Get_Note (Npad.all, Note, Empty_Policy_Manager_Note);

      return Get_Policy_Overrides (Note.Overrides, Types);
   end Get_Policy_Overrides;

   --------------------------
   -- Set_Policy_Overrides --
   --------------------------

   procedure Set_Policy_Overrides
     (Self     : Ref'Class;
      Policies : CORBA.Policy.PolicyList;
      Set_Add  : SetOverrideType)
   is
      Npad    : Notepad_Access;
      Note    : Policy_Manager_Note;
      Indexes : CORBA.Unsigned_Short;

   begin
      if Is_Nil (Self) then
         Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      if PolyORB.CORBA_P.Local.Is_Local (Self) then
         Raise_No_Implement (No_Implement_Members'(Minor     => 3,
                                                   Completed => Completed_No));
      end if;

      Npad :=
        PolyORB.References.Notepad_Of (Internals.To_PolyORB_Ref (Ref (Self)));

      if Set_Add = ADD_OVERRIDE then
         Get_Note (Npad.all, Note, Empty_Policy_Manager_Note);
      end if;

      Add_Policy_Overrides (Note.Overrides, Policies, Reference_Level);

      Check_Compatibility (Note.Overrides, Indexes);

      if Indexes /= 0 then
         raise Program_Error;
         --  XXX should raise the CORBA.InvalidPolicies exception
      end if;

      Set_Note (Npad.all, Note);
   end Set_Policy_Overrides;

   -------------------------
   -- Validate_Connection --
   -------------------------

   procedure Validate_Connection
     (Self                  : Ref;
      Inconsistent_Policies :    out CORBA.Policy.PolicyList;
      Result                :    out CORBA.Boolean)
   is
      pragma Unreferenced (Inconsistent_Policies);

      use PolyORB.Errors;

      The_Servant : PolyORB.Components.Component_Access;
      The_Profile : PolyORB.Binding_Data.Profile_Access;

      Error : Error_Container;

   begin
      PolyORB.References.Binding.Bind
        (CORBA.Object.Internals.To_PolyORB_Ref (Self),
         PolyORB.Setup.The_ORB,
         (others => null),
         The_Servant,
         The_Profile,
         False,
         Error);

      if Found (Error) then
         Result := False;

         if Error.Kind /= ForwardRequest_E then
            PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);

         else
            --  Do not propagate exception if we receive a ForwardRequest
            --  error.

            Catch (Error);
            return;
         end if;
      else
         Result := True;
      end if;
   end Validate_Connection;

end CORBA.Object.Policies;
