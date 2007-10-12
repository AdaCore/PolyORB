------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 2                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

--  CTSC - Client side Thread Scope PICurrent
--  CRSC - Client side Request Scope PICurrent
--  STSC - Server side Thread Scope PICurrent
--  SRSC - Server side Request Scope PICurrent

with CORBA.Object;
with CORBA.ORB;

with PortableInterceptor.Current.Helper;
with PortableInterceptor.ORBInitializer.Initialize_All;
with PortableInterceptor.ORBInitializer.Register;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with PolyORB.Smart_Pointers;
with PolyORB.Utils.Report;

with Test002_Globals;
with Test002_ORB_Initializer.Impl;
with Test002_Interface.Helper;
with Test002_Interface.Impl;

procedure Test002 is

   use CORBA;
   use CORBA.TypeCode;
   use PolyORB.Utils.Report;
   use PortableInterceptor;
   use PortableInterceptor.Current;
   use Test002_Globals;

   Test_Object : Test002_Interface.Ref;

   procedure Init_Test;

   procedure Init_Test is
      use PolyORB.CORBA_P.Server_Tools;

      Ref : CORBA.Object.Ref;
   begin
      CORBA.ORB.Initialize ("ORB");

      declare
         Ptr : constant Test002_ORB_Initializer.Impl.Object_Ptr
           := new Test002_ORB_Initializer.Impl.Object;
         Ref : Test002_ORB_Initializer.Local_Ref;
      begin
         Test002_ORB_Initializer.Set
          (Ref, PolyORB.Smart_Pointers.Entity_Ptr (Ptr));
         PortableInterceptor.ORBInitializer.Register
          (PortableInterceptor.ORBInitializer.Local_Ref (Ref));
      end;
      PortableInterceptor.ORBInitializer.Initialize_All;

      Initiate_Servant (new Test002_Interface.Impl.Object, Ref);
      Test_Object := Test002_Interface.Helper.To_Ref (Ref);

      PI_Current :=
        PortableInterceptor.Current.Helper.To_Local_Ref
          (CORBA.ORB.Resolve_Initial_References
            (CORBA.ORB.To_CORBA_String ("PICurrent")));

      Initiate_Server (True);
   end Init_Test;

begin
   Init_Test;
   New_Test ("PICurrent Thread and Request Scopes Slots");

   declare
      Aux : Any;
   begin
      Aux := Get_Slot (PI_Current, Test_Slot);

      if Get_Type (Aux) /= TC_Null then
         Output ("Uninitialized CTSC slot value (a)", False);
      else
         Output ("Uninitialized CTSC slot value", True);
      end if;

   exception
      when others =>
         Output ("Uninitialized CTSC slot value (b)", False);
   end;

   begin
      Set_Slot (PI_Current, Test_Slot, To_Any (Long (10)));

      declare
         Aux : Any;
      begin
         Aux := Get_Slot (PI_Current, Test_Slot);

         if Get_Type (Aux) /= TC_Long then
            Output ("Setting CTSC slot value", False);
         elsif From_Any (Aux) /= Long (10) then
            Output ("Setting CTSC slot value", False);
         else
            Output ("Setting CTSC slot value", True);
         end if;
      end;

   exception
      when others =>
         Output ("Setting CTSC slot value", False);
   end;

   Test002_Interface.Proc (Test_Object);

   declare
      Aux : Any;
   begin
      Aux := Get_Slot (PI_Current, Test_Slot);

      if Get_Type (Aux) /= TC_Long then
         Output ("Slot value is unchanged in CTSC after invocation", False);
      elsif From_Any (Aux) /= Long (10) then
         Output ("Slot value is unchanged in CTSC after invocation", False);
      else
         Output ("Slot value is unchanged in CTSC after invocation", True);
      end if;

   exception
      when others =>
         Output ("Slot value is unchanged in CTSC after invocation", False);
   end;

   End_Report;
   CORBA.ORB.Shutdown (True);
end Test002;
