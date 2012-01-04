------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . D S A _ P . N A M E _ S E R V I C E . M D N S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010-2012, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with System.RPC;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Binding_Data.Local;
with PolyORB.DSA_P.Exceptions;
with PolyORB.DSA_P.Name_Service.mDNS.Client;
with PolyORB.DSA_P.Name_Service.mDNS.Servant;
with PolyORB.Errors;
with PolyORB.Log;
with PolyORB.Minimal_Servant;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA_Manager;
with PolyORB.POA_Policies.Request_Processing_Policy.Use_Default_Servant;
with PolyORB.POA_Policies.Servant_Retention_Policy.Retain;
with PolyORB.POA_Policies.Id_Assignment_Policy.System;
with PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple;
with PolyORB.POA_Policies.Implicit_Activation_Policy.Activation;
with PolyORB.POA_Policies.Lifespan_Policy.Persistent;
with PolyORB.POA_Types;
with PolyORB.Setup;
with PolyORB.Tasking.Threads;
with PolyORB.Types;

package body PolyORB.DSA_P.Name_Service.mDNS is

   use PolyORB.Any;
   use PolyORB.Any.NVList;
   use PolyORB.Any.NVList.Internals;
   use PolyORB.Any.NVList.Internals.NV_Lists;
   use PolyORB.Log;
   use PolyORB.POA_Policies;
   use PolyORB.POA_Types;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.dsa_p.name_service.mdns");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   Root_DNS : aliased PolyORB.DSA_P.Name_Service.mDNS.Servant.Object_Ptr;
   Root_DNS_Ref : PolyORB.References.Ref;

   ------------------------------
   -- Initialize_MDNS_Policies --
   ------------------------------

   procedure Initialize_MDNS_Policies
     (My_Default_Policies : out PolyORB.POA_Policies.PolicyList)
   is
      use PolyORB.POA_Policies.Policy_Lists;

   begin
      Append (My_Default_Policies,
              Policy_Access
                (Request_Processing_Policy.Use_Default_Servant.Create));
      Append (My_Default_Policies,
              Policy_Access (Servant_Retention_Policy.Retain.Create));
      Append (My_Default_Policies,
              Policy_Access (Id_Assignment_Policy.System.Create));
      Append (My_Default_Policies,
              Policy_Access (Id_Uniqueness_Policy.Multiple.Create));
      Append (My_Default_Policies,
              Policy_Access
              (Implicit_Activation_Policy.Activation.Create));
      Append (My_Default_Policies,
              Policy_Access (Lifespan_Policy.Persistent.Create));
   end Initialize_MDNS_Policies;

   -------------------------
   -- Nameserver_Register --
   -------------------------

   procedure Nameserver_Register
     (Name_Ctx : access MDNS_Name_Server;
      Name : String;
      Kind : String;
      Obj  : PolyORB.References.Ref)
   is
      use PolyORB.DSA_P.Name_Service.mDNS.Servant;
      use PolyORB.References;
      pragma Unreferenced (Name_Ctx);
      Version : PolyORB.Types.String;

   begin
      pragma Debug (C, O ("About to register " & Name & " on mDNS servant"));

      --  In this block we retrieve the Version Id of the package

      declare
         Type_Id    : constant String := Type_Id_Of (Obj);
         Last_Colon : Integer;
      begin
         for C in reverse Type_Id'Range loop
            if Type_Id (C) = ':' then
               Last_Colon := C;
               exit;
            end if;
         end loop;
         Version :=
           To_PolyORB_String (Type_Id (Last_Colon + 1 .. Type_Id'Last));
      end;

      --  We register it within the mDNS default servant

      Append_Entry_To_Context
        (PolyORB.Types.To_PolyORB_String (Name),
         PolyORB.Types.To_PolyORB_String (Kind),
         Version,
         Obj);
   end Nameserver_Register;

   function Nameserver_Lookup
     (Context : access MDNS_Name_Server;
      Name    : String;
      Kind    : String;
      Initial : Boolean := True) return PolyORB.References.Ref
   is
      use PolyORB.Errors;

      Result : PolyORB.References.Ref;
      Retry_Count : Natural := 0;
   begin
      pragma Debug
        (C, O ("Nameserver_Lookup (" & Name & "." & Kind & "): enter"));

      --  Invoke the Resolve procedure which calls the remote object
      --  constructs a local reference as a result and returns it.

      loop
         begin
            --  Unit not known yet, we therefore know that it is remote, and we
            --  need to look it up with the mDNS naming service.

            Result :=
              PolyORB.DSA_P.Name_Service.mDNS.Client.Resolve
                (Context.Base_Ref, Name, Kind);

            if not Is_Reference_Valid (Result) then
               PolyORB.References.Release (Result);
            end if;

         exception
               --  Catch all exceptions: we will retry resolution, and bail
               --  out after Max_Requests iterations.

            when E : others =>
               pragma Debug (C, O ("retry" & Retry_Count'Img & " got "
                 & Ada.Exceptions.Exception_Information (E)));
               PolyORB.References.Release (Result);
         end;

         exit when not (Initial and then PolyORB.References.Is_Nil (Result));
         --  Resolve succeeded, or just trying to refresh a stale ref:
         --  exit loop.

         if Retry_Count = Max_Requests then
            raise System.RPC.Communication_Error with
              "lookup of " & Kind & " " & Name & " failed";
         end if;
         Retry_Count := Retry_Count + 1;
         PolyORB.Tasking.Threads.Relative_Delay (Time_Between_Requests);
      end loop;

      pragma Debug
        (C, O ("Nameserver_Lookup (" & Name & "." & Kind & "): leave"));

      return Result;
   end Nameserver_Lookup;

   procedure Initiate_MDNS_Context
     (MDNS_Reference : String;
      Context        : out PolyORB.DSA_P.Name_Service.Name_Server_Access)
   is
      use PolyORB.Errors;
      use PolyORB.POA;
      use PolyORB.DSA_P.Name_Service.mDNS.Servant;
      use PolyORB.ORB;
      use PolyORB.POA_Manager;
      use type PolyORB.Obj_Adapters.Obj_Adapter_Access;
      pragma Warnings (Off, Context);

      Root_POA : constant POA.Obj_Adapter_Access := POA.Obj_Adapter_Access
                                      (Object_Adapter (PolyORB.Setup.The_ORB));
      DNS_POA  : POA.Obj_Adapter_Access;
      Policies : PolyORB.POA_Policies.PolicyList;
      Error    : Error_Container;
      Oid      : Object_Id_Access;
      Stringified_Ref : PolyORB.Types.String;

   begin
      pragma Assert (Context /= null);

      Initialize_MDNS_Policies (Policies);
      Root_DNS := new PolyORB.DSA_P.Name_Service.mDNS.Servant.Object;
      Create_POA
        (Root_POA,
         "DNS_POA",
         A_POAManager => null,
         Policies     => Policies,
         POA          => DNS_POA,
         Error        => Error);
      Set_Servant
        (DNS_POA,
         Minimal_Servant.To_PolyORB_Servant
           (PolyORB.Minimal_Servant.Servant_Acc (Root_DNS)),
         Error);
      Servant_To_Id (DNS_POA, DNS_POA.Default_Servant, Oid, Error);

      --  Create a local reference designating only our own mDNS servant

      declare
         use PolyORB.Binding_Data.Local;
         LP : constant PolyORB.Binding_Data.Profile_Access :=
                new Local_Profile_Type;
      begin
         Create_Local_Profile (Oid.all, Local_Profile_Type (LP.all));
         PolyORB.References.Create_Reference
           (Profiles => (1 => LP), Type_Id => "", R => Root_DNS_Ref);
      end;

      Stringified_Ref := Types.To_PolyORB_String (MDNS_Reference &
                                  PolyORB.Objects.Oid_To_Hex_String (Oid.all));

      Free (Oid);

      PolyORB.References.String_To_Object
        (Types.To_Standard_String (Stringified_Ref),
         Context.Base_Ref);

      if References.Is_Nil (Context.Base_Ref) then
         raise System.RPC.Communication_Error;
      end if;

      Activate (POAManager_Access
                (PolyORB.POA_Manager.Entity_Of (DNS_POA.POA_Manager)), Error);
      if Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;
      pragma Debug (C, O ("Leaving"));
   end Initiate_MDNS_Context;

   ----------------------
   -- Get_MDNS_Servant --
   ----------------------

   function Get_MDNS_Servant return PolyORB.References.Ref is
   begin
      return Root_DNS_Ref;
   end Get_MDNS_Servant;

end PolyORB.DSA_P.Name_Service.mDNS;
