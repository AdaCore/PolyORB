------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . C O R B A _ P . S E R V E R _ T O O L S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
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

--  Helper functions for CORBA servers. Note that using this unit implies using
--  the Portable Object Adapter.

with CORBA.ORB;
with CORBA.Policy;

with PortableServer.POA.Helper;
with PortableServer.POAManager;

with PolyORB.Log;
with PolyORB.Tasking.Threads;

package body PolyORB.CORBA_P.Server_Tools is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.corba_p.server_tools");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   Root_POA : PortableServer.POA.Local_Ref;

   ------------------
   -- Get_Root_POA --
   ------------------

   function Get_Root_POA return PortableServer.POA.Local_Ref is
   begin
      if PortableServer.POA.Is_Nil (Root_POA) then
         Root_POA := PortableServer.POA.Helper.To_Local_Ref
           (CORBA.ORB.Resolve_Initial_References
              (CORBA.ORB.To_CORBA_String ("RootPOA")));
      end if;
      return Root_POA;
   end Get_Root_POA;

   ---------------------
   -- Initiate_Server --
   ---------------------

   procedure Initiate_Server (Start_New_Task : Boolean := False) is
   begin
      PortableServer.POAManager.Activate
        (PortableServer.POA.Get_The_POAManager (Get_Root_POA));

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
     (S : PortableServer.Servant;
      R : out CORBA.Object.Ref'Class)
   is
   begin
      pragma Debug (O ("Initiate_Servant: enter"));
      CORBA.Object.Set
         (R, CORBA.Object.Object_Of
          (PortableServer.POA.Servant_To_Reference (Get_Root_POA, S)));
      pragma Debug (O ("Initiate_Servant: end"));
   end Initiate_Servant;

   ---------------------------------
   -- Initiate_Well_Known_Service --
   ---------------------------------

   procedure Initiate_Well_Known_Service
     (S    : PortableServer.Servant;
      Name : String;
      R    : out CORBA.Object.Ref'Class)
   is
      use CORBA.Policy.IDL_SEQUENCE_Policy;
      use PortableServer.POA;

      Policies : CORBA.Policy.PolicyList;
      Serv_POA : PortableServer.POA.Local_Ref;
   begin
      Append (Policies,
              CORBA.Policy.Ref (Create_Request_Processing_Policy
                                  (PortableServer.USE_DEFAULT_SERVANT)));
      Append (Policies,
              CORBA.Policy.Ref (Create_Servant_Retention_Policy
                                  (PortableServer.NON_RETAIN)));
      Append (Policies,
              CORBA.Policy.Ref (Create_Id_Assignment_Policy
                                  (PortableServer.USER_ID)));
      Append (Policies,
              CORBA.Policy.Ref (Create_Id_Uniqueness_Policy
                                  (PortableServer.MULTIPLE_ID)));
      Append (Policies,
              CORBA.Policy.Ref (Create_Implicit_Activation_Policy
                                  (PortableServer.NO_IMPLICIT_ACTIVATION)));
      Append (Policies,
              CORBA.Policy.Ref (Create_Lifespan_Policy
                                  (PortableServer.PERSISTENT)));
      Serv_POA := PortableServer.POA.Helper.To_Local_Ref
        (PortableServer.POA.Create_POA
           (Get_Root_POA,
            CORBA.To_CORBA_String (Name),
            PortableServer.POA.Get_The_POAManager (Get_Root_POA),
            Policies));

      PortableServer.POA.Set_Servant (Serv_POA, S);
      CORBA.Object.Set (R, CORBA.Object.Object_Of (
        PortableServer.POA.Create_Reference_With_Id (Serv_POA,
          PortableServer.String_To_ObjectId ("O"),
          PortableServer.Internals.Get_Type_Id (S))));
   end Initiate_Well_Known_Service;

   --------------------------
   -- Reference_To_Servant --
   --------------------------

   procedure Reference_To_Servant
     (R : CORBA.Object.Ref'Class;
      S : out PortableServer.Servant)
   is
   begin
      S := PortableServer.POA.Reference_To_Servant
        (Get_Root_POA, CORBA.Object.Ref (R));
   end Reference_To_Servant;

   --------------------------
   -- Servant_To_Reference --
   --------------------------

   procedure Servant_To_Reference
     (S : PortableServer.Servant;
      R : out CORBA.Object.Ref'Class) renames Initiate_Servant;

end PolyORB.CORBA_P.Server_Tools;
