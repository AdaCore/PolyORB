------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . C O R B A _ P . S E R V E R _ T O O L S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Helper functions for CORBA servers.

with CORBA.Object;
with CORBA.ORB;

with PortableServer.POA.Helper;
with PortableServer.POAManager;

with PolyORB.Log;
with PolyORB.Tasking.Threads;

package body PolyORB.CORBA_P.Server_Tools is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.corba_p.server_tools");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ------------------
   -- Get_Root_POA --
   ------------------

   function Get_Root_POA return PortableServer.POA.Ref is
   begin
      return PortableServer.POA.Helper.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));
   end Get_Root_POA;

   ---------------------
   -- Initiate_Server --
   ---------------------

   procedure Initiate_Server (Start_New_Task : Boolean := False) is
      Root_POA : PortableServer.POA.Ref
        := PortableServer.POA.Helper.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

   begin
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
      R : out CORBA.Object.Ref'Class)
   is
      Root_POA : PortableServer.POA.Ref;

   begin
      pragma Debug (O ("Initiate_Servant : enter"));

      Root_POA := PortableServer.POA.Helper.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

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
      S : out PortableServer.Servant)
   is
      Root_POA : PortableServer.POA.Ref;

   begin
      Root_POA := PortableServer.POA.Helper.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

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
