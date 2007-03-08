------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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
