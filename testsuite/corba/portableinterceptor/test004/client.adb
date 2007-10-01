------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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

with CORBA.Impl;
with CORBA.ORB;
with PolyORB.Utils.Report;
with PortableInterceptor.ORBInitializer.Initialize_All;
with PortableInterceptor.ORBInitializer.Register;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with Test.ClientORBInitializer.Impl;
with Test.Demo;

procedure Client is
begin
   PolyORB.Utils.Report.New_Test ("Adding and retrieving tagged components");

   --  Initialize ORB

   declare
      Argv : CORBA.ORB.Arg_List := CORBA.ORB.Command_Line_Arguments;
   begin
      CORBA.ORB.Init (CORBA.ORB.To_CORBA_String ("ORB"), Argv);
   end;

   --  Register Interceptors

   declare
      Ptr  : constant Test.ClientORBInitializer.Impl.Object_Ptr
        := new Test.ClientORBInitializer.Impl.Object;
      Ref  : PortableInterceptor.ORBInitializer.Local_Ref;

   begin
      PortableInterceptor.ORBInitializer.Set
        (Ref, CORBA.Impl.Object_Ptr (Ptr));

      PortableInterceptor.ORBInitializer.Register (Ref);

      PortableInterceptor.ORBInitializer.Initialize_All;
   end;

   declare
      Ref : Test.Demo.Ref;

   begin
      CORBA.ORB.String_To_Object
        (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)), Ref);

      Test.Demo.shutdown (Ref);
   end;

   PolyORB.Utils.Report.End_Report;
end Client;
