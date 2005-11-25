------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
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

--  XXX should test POA self destruction

with Ada.Exceptions;
with Ada.Text_IO;

with CORBA.ORB;
with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Utils.Report;

with Test_AdapterActivator;
with Test_ServantActivator;
with Test_MyPOA;

with Test000_Setup;

procedure Test000 is

   use Ada.Exceptions;
   use Ada.Text_IO;
   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Utils.Report;

   use Test_AdapterActivator;
   use Test_ServantActivator;
   use Test_MyPOA;

   use Test000_Setup;

begin
   Init_Test;
   Test_Root_POA;
   Test_POAManager;
   Test_Single_Thread_Policy;
   Test_Main_Thread_Policy;
   Test_Conversion (Get_Root_POA);
   Test_POA_Creation;
   Test_POA_API;
   Test_POA_Hierarchy;
   Run_Test_AdapterActivator;
   Run_Test_ServantActivator;
   Run_Test_MyPOA;
   Test_OID;
   End_Report;

   CORBA.ORB.Shutdown (False);

exception
   when E : others =>
      Put_Line ("Got exception "
                & Exception_Name (E)
                & " : "
                & Exception_Message (E));
      Output ("END TESTS", False);

      CORBA.ORB.Shutdown (False);

end Test000;
