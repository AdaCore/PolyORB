------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . C O R B A _ P . S E R V E R _ T O O L S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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
with PortableServer.POA;

package PolyORB.CORBA_P.Server_Tools is

   pragma Elaborate_Body;

   type Hook_Type is access procedure;
   Initiate_Server_Hook : Hook_Type;
   --  Access to a procedure to be called upon start up.
   --  See Initiate_Server for more details.

   procedure Initiate_Server (Start_New_Task : Boolean := False);
   --  Start a new ORB, and initialize the Root POA.
   --  If Start_New_Task is True, a new task will be created and
   --  control will be returned to the caller.  Otherwise, the ORB
   --  will be executing in the current context.
   --  If the Initiate_Server_Hook variable is not null, the
   --  designated procedure will be called after initializing the ORB,
   --  prior to entering the server loop.

   function Get_Root_POA return PortableServer.POA.Ref;
   --  Return the Root_POA attached to the current ORB instance.

   procedure Initiate_Servant
     (S : in PortableServer.Servant;
      R : out CORBA.Object.Ref'Class);
   --  Initiate a servant: register a servant to the Root POA.
   --  If the Root POA has not been initialized, initialize it.

   procedure Reference_To_Servant
     (R : in CORBA.Object.Ref'Class;
      S : out PortableServer.Servant);
   --  Convert a CORBA.Object.Ref into a PortableServer.Servant.

   procedure Servant_To_Reference
     (S : in PortableServer.Servant;
      R : out CORBA.Object.Ref'Class);
   --  Convert a PortableServer.Servant into CORBA.Object.Ref.

end PolyORB.CORBA_P.Server_Tools;
