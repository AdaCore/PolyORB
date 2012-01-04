------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          T E S T _ C O M M O N                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Errors;
with PolyORB.Objects;
with PolyORB.References.IOR;
with PolyORB.Servants;
with PolyORB.Setup;
with PolyORB.Types;
with PolyORB.ORB;
with PolyORB.Utils.Report;

with Test_Servant;

package body Test_Common is

   use PolyORB.Errors;
   use PolyORB.Objects;
   use PolyORB.ORB;
   use PolyORB.Setup;
   use PolyORB.Types;
   use PolyORB.Utils.Report;

   use Test_Servant;

   --------------------
   -- Test_Simple_OA --
   --------------------

   procedure Test_Simple_OA
     (Obj_Adapter : PolyORB.Obj_Adapters.Obj_Adapter_Access)
   is

      S1  : My_Servant_Access;
      My_Id : Object_Id_Access;

      My_Ref : PolyORB.References.Ref;

      Error : Error_Container;

   begin
      --  Create object adapter.

      PolyORB.Obj_Adapters.Create (Obj_Adapter);

      --  Link object adapter with ORB.
      Set_Object_Adapter (The_ORB, Obj_Adapter);

      Output ("Created Object Adapter", True);

      --  Create Servant.
      S1 := new My_Servant;
      S1.Nb    := 1;
      S1.Name  := To_PolyORB_String ("Servant1");
      Output ("Servant Created", True);

      PolyORB.Obj_Adapters.Export
        (Obj_Adapter,
         PolyORB.Servants.Servant_Access (S1),
         null,
         My_Id,
         Error);
      --  Register it with the SOA.

      if Found (Error) then
         raise Program_Error;
      end if;

      Create_Reference (The_ORB, My_Id, "POLYORB:TEST_SERVANT:1.0", My_Ref);
      --  Obtain object reference.

      Output ("Registered object", True);

      declare
         IOR : constant String :=
           PolyORB.References.IOR.Object_To_String (My_Ref);
         pragma Warnings (Off);
         pragma Unreferenced (IOR);
         pragma Warnings (On);
      begin
         Output ("IOR created", True);
      end;

      PolyORB.Obj_Adapters.Unexport
        (Obj_Adapter,
         My_Id,
         Error);

      if Found (Error) then
         raise Program_Error;
      end if;

      Output ("Unregistered object", True);

      --  Destroy object adapter

      PolyORB.Obj_Adapters.Destroy (Obj_Adapter);
      Output ("Destroyed Object Adapter", True);

   end Test_Simple_OA;

end Test_Common;
