------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          T E S T _ C O M M O N                           --
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
