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
with PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA.Basic_POA;
with PolyORB.POA_Config;
with PolyORB.POA_Manager;
with PolyORB.Setup;
with PolyORB.Smart_Pointers;
with PolyORB.Soft_Links;
with PolyORB.Types;

with PolyORB.POA_Config.Minimum;
--  The configuration for the root POA.

with PolyORB.POA_Config.Proxies;
--  XXX should be depended upon only when proxies are desired.

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
   use PolyORB.POA.Basic_POA;
   use type PolyORB.POA.Obj_Adapter_Access;

   package L is new PolyORB.Log.Facility_Log ("polyorb.corba_p.server_tools");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Root_POA : PortableServer.POA.Ref;
   Root_POA_Object : POA.Obj_Adapter_Access;

   procedure Initiate_RootPOA;
   procedure Initiate_Proxies_POA;

   ----------------------
   -- Initiate_RootPOA --
   ----------------------

   procedure Initiate_RootPOA is
   begin
      pragma Assert (Root_POA_Object = null);
      pragma Debug (O ("Initializing default POA configuration..."));
      POA_Config.Set_Configuration
        (new POA_Config.Minimum.Minimum_Configuration);

      pragma Debug (O ("Initializing root POA..."));
      Root_POA_Object := new POA.Basic_POA.Basic_Obj_Adapter;
      POA.Basic_POA.Create
        (Basic_Obj_Adapter (Root_POA_Object.all)'Access);

      ORB.Set_Object_Adapter
        (Setup.The_ORB, Obj_Adapters.Obj_Adapter_Access (Root_POA_Object));
      --  Link object adapter with ORB.

      PortableServer.POA.Set
        (Root_POA, Smart_Pointers.Entity_Ptr (Root_POA_Object));

      Initiate_Proxies_POA;

   end Initiate_RootPOA;

   --------------------------
   -- Initiate_Proxies_POA --
   --------------------------

   Proxies_POA_Configuration :
     PolyORB.POA_Config.Proxies.Proxies_Configuration;

   procedure Initiate_Proxies_POA is
      use PolyORB.POA_Manager;
   begin
      pragma Assert (Root_POA_Object /= null);
      POA.Basic_POA.Set_Proxies_OA
        (POA.Basic_POA.Basic_Obj_Adapter_Access (Root_POA_Object),
         POA.Basic_POA.Basic_Obj_Adapter_Access
         (POA.Basic_POA.Create_POA
          (Basic_Obj_Adapter (Root_POA_Object.all)'Access,
           Types.To_PolyORB_String ("Proxies"),
           POAManager_Access (Entity_Of (Root_POA_Object.POA_Manager)),
           POA_Config.Default_Policies
           (POA_Config.Configuration_Type'Class
            (Proxies_POA_Configuration)))));
   end Initiate_Proxies_POA;

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

      if Start_New_Task then
         PolyORB.Soft_Links.Create_Task (CORBA.ORB.Run'Access);
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
