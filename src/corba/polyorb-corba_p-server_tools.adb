------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . C O R B A _ P . S E R V E R _ T O O L S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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

with PolyORB.Log;
with PolyORB.Obj_Adapters;
with PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA.Basic_POA;
with PolyORB.POA_Config;
with PolyORB.Setup;
with PolyORB.Smart_Pointers;
with PolyORB.Tasking.Threads;

with PolyORB.POA_Config.Root_POA;
--  The configuration for the RootPOA.

with PolyORB.Setup.Proxies_POA;
--  XXX should be depended upon only when proxies are desired.

with CORBA.AbstractBase;
pragma Warnings (Off, CORBA.AbstractBase);

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);

with CORBA.Object;
--  pragma Warnings (Off, CORBA.Object);

with CORBA.ORB;

with PortableServer.POA;
pragma Elaborate_All (PortableServer.POA);

with PortableServer.POAManager;

package body PolyORB.CORBA_P.Server_Tools is

   use PolyORB.Log;
   use PolyORB.POA.Basic_POA;
   use type PolyORB.POA.Obj_Adapter_Access;

   package L is new PolyORB.Log.Facility_Log ("polyorb.corba_p.server_tools");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Initiate_RootPOA;

   ----------------------
   -- Initiate_RootPOA --
   ----------------------

   Root_POA        : PortableServer.POA.Ref;
   Root_POA_Object : PolyORB.POA.Obj_Adapter_Access;

   procedure Initiate_RootPOA is
   begin
      pragma Assert (Root_POA_Object = null);
      pragma Debug (O ("Initializing Root POA configuration..."));
      PolyORB.POA_Config.Set_Configuration
        (new PolyORB.POA_Config.Root_POA.Root_POA_Configuration);

      pragma Debug (O ("Initializing Root POA..."));
      Root_POA_Object := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
      PolyORB.POA.Create (Root_POA_Object);

      --  Link object adapter with ORB.

      PolyORB.ORB.Set_Object_Adapter
        (PolyORB.Setup.The_ORB,
         PolyORB.Obj_Adapters.Obj_Adapter_Access (Root_POA_Object));

      PortableServer.POA.Set
        (Root_POA, Smart_Pointers.Entity_Ptr (Root_POA_Object));

      PolyORB.Setup.Proxies_POA (Root_POA_Object);

      --  Register initial reference for "RootPOA".

      pragma Debug (O ("Registering Root POA initial reference."));

      CORBA.ORB.Register_Initial_Reference
        (CORBA.ORB.To_CORBA_String ("RootPOA"),
         CORBA.Object.Ref (Root_POA));
   end Initiate_RootPOA;

   ------------------
   -- Get_Root_POA --
   ------------------

   function Get_Root_POA
     return PortableServer.POA.Ref is
   begin
      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Root_POA)) then
         Initiate_RootPOA;
      end if;

      return PortableServer.POA.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));
   end Get_Root_POA;

   ---------------------
   -- Initiate_Server --
   ---------------------

   procedure Initiate_Server (Start_New_Task : Boolean := False)
   is
   begin
      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Root_POA)) then
         Initiate_RootPOA;
      end if;

      PortableServer.POAManager.Activate
        (PortableServer.POA.Get_The_POAManager (Root_POA));

      if Initiate_Server_Hook /= null then
         Initiate_Server_Hook.all;
      end if;

      if Start_New_Task then
         PolyORB.Tasking.Threads.Create_Task (CORBA.ORB.Run'Access);
      else
         CORBA.ORB.Run;
      end if;
   end Initiate_Server;

   ----------------------
   -- Initiate_Servant --
   ----------------------

   procedure Initiate_Servant
     (S : in PortableServer.Servant;
      R : out CORBA.Object.Ref'Class) is
   begin
      pragma Debug (O ("Initiate_Servant : enter"));

      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Root_POA)) then
         Initiate_RootPOA;
      end if;

      pragma Debug (O ("Initiate_Servant : ready to "
                       & "call CORBA.Object.Set"));

      CORBA.Object.Set
        (R, CORBA.Object.Object_Of
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
