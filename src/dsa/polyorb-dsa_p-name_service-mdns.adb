------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . D S A _ P . N A M E _ S E R V I C E . M D N S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2010, Free Software Foundation, Inc.             --
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

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Log;
with PolyORB.Utils;
with PolyORB.Errors;
with PolyORB.POA;
with PolyORB.Setup;
with PolyORB.ORB;
with PolyORB.DSA_P.Exceptions;
with PolyORB.POA_Policies.Request_Processing_Policy.Use_Default_Servant;
with PolyORB.POA_Policies.Servant_Retention_Policy.Retain;
with PolyORB.POA_Policies.Id_Assignment_Policy.System;
with PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple;
with PolyORB.POA_Policies.Implicit_Activation_Policy.Activation;
with PolyORB.POA_Policies.Lifespan_Policy.Persistent;
with PolyORB.POA_Manager;
with PolyORB.Obj_Adapters;
with PolyORB.Minimal_Servant;
with PolyORB.Types;
with PolyORB.DSA_P.Name_Service.mDNS.Client;
with PolyORB.DSA_P.Name_Service.mDNS.Servant;

package body PolyORB.DSA_P.Name_Service.mDNS is

   use PolyORB.POA_Policies;
   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Any.NVList;
   use PolyORB.Any.NVList.Internals;
   use PolyORB.Any.NVList.Internals.NV_Lists;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.dsa_p.name_service.mdns");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   Root_DNS     : aliased PolyORB.DSA_P.Name_Service.mDNS.Servant.
     Object_Ptr;

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
     (Name_Ctx : access MDNS_Name_Context;
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
     (Context : access MDNS_Name_Context;
      Name    : String;
      Kind    : String;
      Initial : Boolean := True) return PolyORB.References.Ref
   is
      use PolyORB.Errors;
      use PolyORB.Utils;

      LName : constant String := To_Lower (Name);
      pragma Unreferenced (Initial, LName);
      Result : PolyORB.References.Ref;
   begin
      pragma Debug
        (C, O ("Nameserver_Lookup (" & Name & "." & Kind & "): enter"));

      --  Unit not known yet, we therefore know that it is remote, and we
      --  need to look it up with the mDNS naming service.

      --  We create the remote reference from the stringified Ref

      PolyORB.References.String_To_Object
        (Types.To_Standard_String (Context.Stringified_Ref),
         Context.Base_Ref);

      if Context.Base_Ref.Is_Null then
         pragma Debug (C, O ("Target is null"));
         raise Program_Error;
      end if;

      --  Invoke the Resolve procedure which calls the remote object
      --  constructs a local reference as a result and returns it.

      Result :=
        PolyORB.DSA_P.Name_Service.mDNS.Client.Resolve
          (Context.Base_Ref, Name, Kind);

      pragma Debug
        (C, O ("Nameserver_Lookup (" & Name & "." & Kind & "): leave"));

      return Result;
   end Nameserver_Lookup;

   procedure Initiate_MDNS_Context
     (MDNS_Reference : String;
      Context : out PolyORB.DSA_P.Name_Service.Name_Context_Access;
      Oid : out PolyORB.Objects.Object_Id_Access)
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
      DNS_POA : POA.Obj_Adapter_Access;
      Policies : PolyORB.POA_Policies.PolicyList;
      Error : Error_Container;

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
      Set_Servant (DNS_POA, Minimal_Servant.To_PolyORB_Servant
                   (PolyORB.Minimal_Servant.Servant_Acc (Root_DNS)), Error);
      Servant_To_Id (DNS_POA, DNS_POA.Default_Servant, Oid, Error);
      Context.Stringified_Ref := Types.To_PolyORB_String (MDNS_Reference &
                                  PolyORB.Objects.Oid_To_Hex_String (Oid.all));
      Activate (POAManager_Access
                (PolyORB.POA_Manager.Entity_Of (DNS_POA.POA_Manager)), Error);
      if Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;
      pragma Debug (C, O ("Leaving"));
   end Initiate_MDNS_Context;

   function Get_MDNS_Servant return PolyORB.References.Ref
   is
   begin
      return PolyORB.DSA_P.Name_Service.Get_Name_Context.Base_Ref;
   end Get_MDNS_Servant;

end PolyORB.DSA_P.Name_Service.mDNS;
