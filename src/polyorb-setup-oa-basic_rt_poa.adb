------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . S E T U P . O A . B A S I C _ R T _ P O A         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2021, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;

with PolyORB.Errors;
with PolyORB.Obj_Adapters;
with PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA_Config.Root_POA;
--  The configuration for the RootPOA.

with PolyORB.POA_Manager;
with PolyORB.RT_POA.Basic_RT_POA;
with PolyORB.Setup.Proxies_POA;
--  XXX should be depended upon only when proxies are desired.

with PolyORB.Utils.Strings;

package body PolyORB.Setup.OA.Basic_RT_POA is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      use PolyORB.Errors;

      Root_POA_Object : PolyORB.POA.Obj_Adapter_Access;

      Error : Error_Container;
   begin
      PolyORB.POA_Config.Set_Configuration
        (new PolyORB.POA_Config.Root_POA.Root_POA_Configuration);

      Root_POA_Object := new PolyORB.RT_POA.Basic_RT_POA.Basic_RT_Obj_Adapter;
      PolyORB.POA.Create (Root_POA_Object);

      PolyORB.POA_Manager.Activate
        (PolyORB.POA_Manager.POAManager_Access
         (PolyORB.POA_Manager.Entity_Of (Root_POA_Object.POA_Manager)),
         Error);

      if Found (Error) then
         Catch (Error);
         raise Program_Error;
      end if;

      --  Link object adapter with ORB.

      PolyORB.ORB.Set_Object_Adapter
        (PolyORB.Setup.The_ORB,
         PolyORB.Obj_Adapters.Obj_Adapter_Access (Root_POA_Object));

      PolyORB.Setup.Proxies_POA
        (Root_POA_Object,
         Error);

      if Found (Error) then
         Catch (Error);
         raise Program_Error;
      end if;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"rt_basic_poa",
       Conflicts => Empty,
       Depends   => +"orb",
       Provides  => +"object_adapter"
       & "poa"
       & "rt_poa",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Setup.OA.Basic_RT_POA;
