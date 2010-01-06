------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2007, Free Software Foundation, Inc.          --
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
