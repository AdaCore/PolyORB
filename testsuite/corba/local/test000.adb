------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with CORBA.Object;
with CORBA.ORB;

with PortableServer.POA.Helper;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with PolyORB.Utils.Report; use PolyORB.Utils.Report;

procedure Test000 is
   POA : PortableServer.POA.Local_Ref;

begin
   CORBA.ORB.Initialize ("ORB");

   begin
      declare
         Int : constant CORBA.Object.Ref'Class
           := CORBA.Object.Get_Interface (CORBA.Object.Ref (POA));

         pragma Warnings (Off);
         pragma Unreferenced (Int);
         pragma Warnings (On);

      begin
         Output ("Test on uninitialized ref", False);
      end;
   exception
      when CORBA.Inv_Objref =>
         Output ("Test on uninitialized ref", True);

      when others =>
         Output ("Test on uninitialized ref", False);
   end;

   POA := PortableServer.POA.Helper.To_Local_Ref
     (CORBA.ORB.Resolve_Initial_References
      (CORBA.ORB.To_CORBA_String ("RootPOA")));

   begin
      declare
         Int : constant CORBA.Object.Ref'Class
           := CORBA.Object.Get_Interface (CORBA.Object.Ref (POA));

         pragma Warnings (Off);
         pragma Unreferenced (Int);
         pragma Warnings (On);

      begin
         Output ("Test on initialized ref", False);
      end;
   exception
      when CORBA.No_Implement =>
         Output ("Test on initialized ref", True);

      when others =>
         Output ("Test on initialized ref", False);
   end;

   End_Report;

exception
   when others =>
      Output ("Unexpected exception", False);
      End_Report;
end Test000;
