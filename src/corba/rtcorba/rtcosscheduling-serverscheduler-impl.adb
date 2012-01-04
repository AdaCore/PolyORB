------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  RTCOSSCHEDULING.SERVERSCHEDULER.IMPL                    --
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

with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.Parameters.File;
with PolyORB.POA;
with PolyORB.POA_Types;
with PolyORB.References.Binding;
with PolyORB.RT_POA;
with PolyORB.RT_POA_Policies.Priority_Model_Policy;
with PolyORB.Servants;
with PolyORB.Setup;
with PolyORB.Tasking.Priorities;
with PolyORB.Types;
with PolyORB.Utils;

with PolyORB.CORBA_P.Exceptions;

with PolyORB.RTCORBA_P.To_ORB_Priority;

with CORBA.ORB;
with RTCORBA.RTORB.Helper;
with RTCORBA.PriorityModelPolicy;
with RTCORBA.ThreadpoolPolicy;

with RTCosScheduling.Helper;

package body RTCosScheduling.ServerScheduler.Impl is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("rtcosscheduling.serverscheduler.impl");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   -----------------------------
   -- Load_Configuration_File --
   -----------------------------

   procedure Load_Configuration_File (Conf_File_Name : String) is
   begin
      PolyORB.Parameters.File.Load_Configuration_File (Conf_File_Name);
   end Load_Configuration_File;

   ----------------
   -- Create_POA --
   ----------------

   function Create_POA
     (Self         : access Object;
      Parent       : PortableServer.POA.Local_Ref;
      Adapter_Name : CORBA.String;
      A_POAManager : PortableServer.POAManager.Local_Ref;
      Policies     : CORBA.Policy.PolicyList)
     return PortableServer.POA.Local_Ref
   is
      pragma Unreferenced (Self);

      RT_ORB : constant RTCORBA.RTORB.Local_Ref
        := RTCORBA.RTORB.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RTORB")));

      All_Policies : CORBA.Policy.PolicyList := Policies;

      Priority_Model_Policy_Ref : RTCORBA.PriorityModelPolicy.Local_Ref;
      Thread_Pool_Policy_Ref : RTCORBA.ThreadpoolPolicy.Local_Ref;

   begin
      pragma Debug (C, O ("Configuring POA "
                       & CORBA.To_String (Adapter_Name)));

      --  Retrieve parameters for the PriorityModel Policy, if any

      declare
         Priority_Model : constant String :=
           PolyORB.Utils.To_Upper
           (PolyORB.Parameters.Get_Conf
            ("poa " & CORBA.To_String (Adapter_Name), "priority_model"));

         Default_Priority : constant Integer :=
           PolyORB.Parameters.Get_Conf
           ("poa " & CORBA.To_String (Adapter_Name), "default_priority");

      begin
         if Priority_Model = "CLIENT_PROPAGATED" then
            pragma Debug (C, O ("Configuring CLIENT_PROPAGATED policy"));

            Priority_Model_Policy_Ref
              := RTCORBA.RTORB.Create_Priority_Model_Policy
              (RT_ORB,
               RTCORBA.CLIENT_PROPAGATED,
               RTCORBA.Priority (Default_Priority));

            CORBA.Policy.IDL_SEQUENCE_Policy.Append
              (All_Policies, CORBA.Policy.Ref (Priority_Model_Policy_Ref));

         elsif Priority_Model = "SERVER_DECLARED" then
            pragma Debug (C, O ("Configuring SERVER_DECLARED policy"));

            Priority_Model_Policy_Ref
              := RTCORBA.RTORB.Create_Priority_Model_Policy
              (RT_ORB,
               RTCORBA.SERVER_DECLARED,
               RTCORBA.Priority (Default_Priority));

            CORBA.Policy.IDL_SEQUENCE_Policy.Append
              (All_Policies, CORBA.Policy.Ref (Priority_Model_Policy_Ref));

         else
            pragma Debug (C, O ("No PriorityModel policy to configure"));
            null;

         end if;
      end;

      --  Retrieve parameters for the ThreadPool Policy, if any

      declare
         Threadpool_Id : constant Integer :=
           PolyORB.Parameters.Get_Conf
           ("poa " & CORBA.To_String (Adapter_Name), "threadpool_id", -1);
      begin
         if Threadpool_Id /= -1 then
            pragma Debug (C, O ("Create Threadpool policy"));

            Thread_Pool_Policy_Ref
              := RTCORBA.RTORB.Create_Threadpool_Policy
              (RT_ORB, RTCORBA.ThreadpoolId (Threadpool_Id));

            CORBA.Policy.IDL_SEQUENCE_Policy.Append
              (All_Policies, CORBA.Policy.Ref (Thread_Pool_Policy_Ref));

         else
            pragma Debug (C, O ("No ThreadPool policy to configure"));
            null;

         end if;
      end;

      return PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Parent,
          Adapter_Name,
          A_POAManager,
          All_Policies));
   end Create_POA;

   ---------------------
   -- Schedule_Object --
   ---------------------

   procedure Schedule_Object
     (Self : access Object;
      Obj  : CORBA.Object.Ref;
      Name : CORBA.String)
   is
      pragma Unreferenced (Self);

      use PolyORB.Errors;
      use PolyORB.RT_POA_Policies.Priority_Model_Policy;

      CORBA_Priority : Integer;

      ORB_Priority : PolyORB.Tasking.Priorities.ORB_Priority;

      The_POA : PolyORB.POA.Obj_Adapter_Access;
      The_Servant : PolyORB.Components.Component_Access;
      The_Profile : PolyORB.Binding_Data.Profile_Access;
      Error : Error_Container;
      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;

   begin
      pragma Debug (C, O ("Configuring object " & CORBA.To_String (Name)));

      --  Retrieve CORBA Priority

      CORBA_Priority := PolyORB.Parameters.Get_Conf
        ("object " & CORBA.To_String (Name), "priority");

      pragma Debug (C, O ("Set priority to:" & CORBA_Priority'Img));

      --  Compute corresponding Native priority

      ORB_Priority := PolyORB.RTCORBA_P.To_ORB_Priority
        (RTCORBA.Priority (CORBA_Priority));

      --  Retrieve servant from reference information

      PolyORB.References.Binding.Bind
        (CORBA.Object.Internals.To_PolyORB_Ref (Obj),
         PolyORB.Setup.The_ORB,
         (others => null),
         The_Servant,
         The_Profile,
         True,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      --  Retrieve POA from reference information

      PolyORB.POA_Types.Oid_To_U_Oid
        (PolyORB.Binding_Data.Get_Object_Key (The_Profile.all).all,
         U_Oid, Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      PolyORB.POA.Find_POA
        (PolyORB.POA.Obj_Adapter
         (PolyORB.ORB.Object_Adapter (PolyORB.Setup.The_ORB).all)'Access,
         PolyORB.Types.To_Standard_String (U_Oid.Creator),
         False,
         The_POA,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      --  Update servant priority information

      Set_Servant_Priority_Information
        (PolyORB.RT_POA.RT_Obj_Adapter (The_POA.all).Priority_Model_Policy.all,
         PolyORB.Servants.Servant_Access (The_Servant),
         ORB_Priority,
         PolyORB.Tasking.Priorities.External_Priority (CORBA_Priority),
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

   exception
      when others =>
         --  For now, we cannot distinguish between an inconsistent
         --  value of the priority, and an unknown activity.

         RTCosScheduling.Helper.Raise_UnknownName
           (UnknownName_Members'
            (CORBA.IDL_Exception_Members with null record));
   end Schedule_Object;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return
        CORBA.Is_Equivalent
          (Logical_Type_Id, RTCosScheduling.ServerScheduler.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

end RTCosScheduling.ServerScheduler.Impl;
