------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with Ada.Command_Line;

with CORBA.DomainManager;
with CORBA.Object.Policies;
with CORBA.ORB;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.Utils.Report;

procedure Client is
   Ref : CORBA.Object.Ref;
   M   : CORBA.DomainManager.DomainManagersList;

begin
   PolyORB.Utils.Report.New_Test ("CORBA::DomainManager");

   CORBA.ORB.Initialize ("ORB");

   CORBA.ORB.String_To_Object
     (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)), Ref);

   begin
      M := CORBA.Object.Policies.Get_Domain_Managers (Ref);
      PolyORB.Utils.Report.Output ("Getting domain managers list", True);

   exception
      when others =>
         PolyORB.Utils.Report.Output ("Getting domain managers list", False);
   end;

   PolyORB.Utils.Report.Output
     ("Domain managers list not empty",
      CORBA.DomainManager.IDL_SEQUENCE_DomainManager.Length (M) /= 0);

   begin
      PolyORB.Utils.Report.Output
        ("Domain manager not null",
         not CORBA.DomainManager.Is_Nil
         (CORBA.DomainManager.IDL_SEQUENCE_DomainManager.Get_Element (M, 1)));

   exception
      when others =>
         PolyORB.Utils.Report.Output ("Domain manager not null", False);
   end;

   PolyORB.Utils.Report.End_Report;
end Client;
