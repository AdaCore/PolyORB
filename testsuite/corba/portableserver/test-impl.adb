------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            T E S T . I M P L                             --
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
