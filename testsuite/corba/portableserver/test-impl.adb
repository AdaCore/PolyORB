------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            T E S T . I M P L                             --
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

with PortableServer.Current;
with PortableServer.POA;

with Test.Skel;
pragma Warnings (Off, Test.Skel);

with Test_Globals;

package body Test.Impl is

   use CORBA.Object;
   use PortableServer;
   use PortableServer.Current;
   use PortableServer.POA;
   use Test_Globals;

   ----------
   -- Proc --
   ----------

   procedure Proc (Self : access Object) is
      pragma Unreferenced (Self);

   begin
      --  PortableServer::Current::get_poa test

      declare
         Aux : PortableServer.POA.Local_Ref;

      begin
         begin
            Aux := PortableServer.POA.Convert.To_Ref (Get_POA (Test_Current));

            if not Is_Equivalent (Test_POA, Aux) then
               Get_POA_Success := False;
            end if;

         exception
            when others =>
               Get_POA_Success := False;
         end;
      end;

      --  PortableServer::Current::get_reference test

      declare
         Aux : CORBA.Object.Ref;

      begin
         Aux := Get_Reference (Test_Current);

         if not Is_Equivalent (Aux, Test_Reference) then
            Get_Reference_Success := False;
         end if;

      exception
         when others =>
            Get_Reference_Success := False;
      end;

      --  PortableServer::Current::get_servant test

      declare
         Aux : PortableServer.Servant;

      begin
         Aux := Get_Servant (Test_Current);

         if Aux /= Test_Servant then
            Get_Servant_Success := False;
         end if;

      exception
         when others =>
            Get_Servant_Success := False;
      end;

      --  PortableServer::Current::get_object_id test

      declare
         Aux : PortableServer.ObjectId;

      begin
         Aux := Get_Object_Id (Test_Current);

         if Aux /= Test_Id then
            Get_Object_Id_Success := False;
         end if;

      exception
         when others =>
            Get_Object_Id_Success := False;
      end;
   end Proc;

end Test.Impl;
