------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . C O R B A _ P . S E R V E R _ T O O L S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Helper functions for CORBA servers. Note that using this unit implies
--  using the Portable Object Adapter.

with CORBA.Object;
with PortableServer.POA;

package PolyORB.CORBA_P.Server_Tools is

   pragma Elaborate_Body;

   type Hook_Type is access procedure;
   Initiate_Server_Hook : Hook_Type;
   --  Access to a procedure to be called upon start up.
   --  See Initiate_Server for more details.

   procedure Activate_Server;
   --  Start a new ORB, and initialize the Root POA.
   --
   --  If the Initiate_Server_Hook variable is not null, the designated
   --  procedure will be called after initializing the ORB.

   procedure Initiate_Server (Start_New_Task : Boolean := False);
   --  Calls Activate_Server then starts ORB main loop.
   --  If Start_New_Task is True, a new task will be created and control will
   --  be returned to the caller. Otherwise, the ORB main loop will be executed
   --  in the current context.

   function Get_Root_POA return PortableServer.POA.Local_Ref;
   --  Return the Root_POA attached to the current ORB instance.

   procedure Initiate_Servant
     (S : PortableServer.Servant;
      R : out CORBA.Object.Ref'Class);
   --  Initiate a servant: register a servant to the Root POA.
   --  If the Root POA has not been initialized, initialize it.

   procedure Reference_To_Servant
     (R : CORBA.Object.Ref'Class;
      S : out PortableServer.Servant);
   --  Convert a CORBA.Object.Ref into a PortableServer.Servant.

   procedure Servant_To_Reference
     (S : PortableServer.Servant;
      R : out CORBA.Object.Ref'Class);
   --  Convert a PortableServer.Servant into CORBA.Object.Ref.

   procedure Initiate_Well_Known_Service
     (S    : PortableServer.Servant;
      Name : String;
      R    : out CORBA.Object.Ref'Class);
   --  Make S accessible through a reference appropriate for
   --  generation of a corbaloc URI with a named key of Name.

end PolyORB.CORBA_P.Server_Tools;
