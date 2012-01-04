------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1999-2012, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;

with CORBA.Object;
with CORBA.ORB;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.Repository.Impl;
with CORBA.Repository_Root;

with PortableServer;

with PolyORB.CORBA_P.Server_Tools;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

procedure Server is
   use CORBA.Repository_Root;
   use PolyORB.CORBA_P.Server_Tools;

begin

   CORBA.ORB.Initialize ("ORB");

   declare
      Ref : CORBA.Object.Ref;
      Repo : constant Repository.Impl.Object_Ptr := new Repository.Impl.Object;

   begin

      Repository.Impl.Init (Repo,
                            IRObject.Impl.Object_Ptr (Repo),
                            dk_Repository,
                            Contained.Impl.Contained_Seq.Null_Sequence);
      Initiate_Servant (PortableServer.Servant (Repo), Ref);

      Ada.Text_IO.Put_Line
        ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
         "'");

      Initiate_Server;
   end;
end Server;
