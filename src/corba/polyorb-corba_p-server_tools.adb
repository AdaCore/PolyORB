------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . C O R B A _ P . S E R V E R _ T O O L S          --
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

--  Helper functions for CORBA servers.

--  $Id$

with PolyORB.Obj_Adapters;
with PolyORB.POA;
with PolyORB.POA.Basic_POA; use PolyORB.POA.Basic_POA;
with PolyORB.POA_Config;
with PolyORB.POA_Config.Minimum;
--  XXX hardcoded configuration!!!!!!
with PolyORB.Setup;
with PolyORB.Setup.Test; use PolyORB.Setup.Test;
with PolyORB.Smart_Pointers;
with PolyORB.No_Tasking;
--  XXX hardcoded tasking policy!!!
with PolyORB.ORB.Task_Policies;

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);
with CORBA.Object;
pragma Warnings (Off, CORBA.Object);
with CORBA.AbstractBase;
pragma Warnings (Off, CORBA.AbstractBase);

with CORBA;
with CORBA.ORB;

with PortableServer.POA;
with PortableServer.POAManager;
pragma Elaborate_All (PortableServer.POA);

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

package body PolyORB.CORBA_P.Server_Tools is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.corba_p.server_tools");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Root_POA : PortableServer.POA.Ref;

--    task type ORBTask is
--       pragma Storage_Size
--         (PolyORB.CORBA_P.Parameters.Server_Tasks_Storage_Size);
--    end ORBTask;
--    type ORBTaskPtr is access ORBTask;

--    task body ORBTask is
--    begin
--       CORBA.ORB.Run;
--    end ORBTask;

   procedure Initiate_RootPOA;

   -----------
   -- Debug --
   -----------


   Setup_Done : Boolean := False;

   procedure Ensure_Setup;

   procedure Ensure_Setup is
   begin
      if Setup_Done then
         return;
      end if;
      Setup_Done := True;

      Initialize_Test_Server
        (PolyORB.No_Tasking.Initialize'Access,
         new PolyORB.ORB.Task_Policies.No_Tasking);


      Initialize_Test_Access_Points;
   end Ensure_Setup;

   ----------------------
   -- Initiate_RootPOA --
   ----------------------

   procedure Initiate_RootPOA is
      --  RootPOAStr  : CORBA.String;
      Obj_Adapter : PolyORB.POA.Obj_Adapter_Access;
   begin
      Ensure_Setup;

      pragma Debug (O ("Initializing OA configuration... "));
      PolyORB.POA_Config.Set_Configuration
        (new PolyORB.POA_Config.Minimum.Minimum_Configuration);
      pragma Debug (O ("Creating object adapter... "));
      Obj_Adapter := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
      PolyORB.POA.Basic_POA.Create
        (Basic_Obj_Adapter (Obj_Adapter.all)'Access);
      --  Create object adapter

      PolyORB.ORB.Set_Object_Adapter
        (PolyORB.Setup.The_ORB,
         PolyORB.Obj_Adapters.Obj_Adapter_Access (Obj_Adapter));
      --  Link object adapter with ORB.

      PortableServer.POA.Set
        (Root_POA, PolyORB.Smart_Pointers.Entity_Ptr (Obj_Adapter));

--       RootPOAStr := CORBA.To_CORBA_String ("RootPOA");
--       Root_POA   := PortableServer.POA.To_Ref
--         (CORBA.ORB.Resolve_Initial_References
--          (CORBA.ORB.ObjectId (RootPOAStr)));
      --  XXX Should REGISTER initial ref for the root POA.
   end Initiate_RootPOA;

   ---------------------
   -- Initiate_Server --
   ---------------------

   procedure Initiate_Server (Start_New_Task : Boolean := True)
   is
      --  ORBMainLoop : ORBTaskPtr;
   begin
      Ensure_Setup;

      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Root_POA)) then
         Initiate_RootPOA;
      end if;

      PortableServer.POAManager.Activate
        (PortableServer.POA.Get_The_POAManager (Root_POA));

      --  if Start_New_Task then
      --     ORBMainLoop := new ORBTask;
      --  else
      CORBA.ORB.Run;
      --  end if;
   end Initiate_Server;

   ----------------------
   -- Initiate_Servant --
   ----------------------

   procedure Initiate_Servant
     (S : in PortableServer.Servant;
      R : out CORBA.Object.Ref'Class) is
   begin
      pragma Debug (O ("Initiate_Servant : enter"));
      Ensure_Setup;

      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Root_POA)) then
         Initiate_RootPOA;
      end if;
      pragma Debug (O ("Initiate_Servant : ready to "
                       & "call CORBA.Object.Set"));
      CORBA.Object.Set
        (CORBA.Object.Ref (R),
         CORBA.Object.Object_Of
         (PortableServer.POA.Servant_To_Reference (Root_POA, S)));
      pragma Debug (O ("Initiate_Servant : end"));
   end Initiate_Servant;

   --------------------------
   -- Reference_To_Servant --
   --------------------------

   procedure Reference_To_Servant
     (R : in CORBA.Object.Ref'Class;
      S : out PortableServer.Servant) is
   begin
      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Root_POA)) then
         Initiate_RootPOA;
      end if;

      S := PortableServer.POA.Reference_To_Servant
        (Root_POA, CORBA.Object.Ref (R));
   end Reference_To_Servant;

   --------------------------
   -- Servant_To_Reference --
   --------------------------

   procedure Servant_To_Reference
     (S : in PortableServer.Servant;
      R : out CORBA.Object.Ref'Class) renames Initiate_Servant;

end PolyORB.CORBA_P.Server_Tools;
