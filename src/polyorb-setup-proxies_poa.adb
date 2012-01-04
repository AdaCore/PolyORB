------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . S E T U P . P R O X I E S _ P O A             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Parameters;
with PolyORB.POA_Manager;
with PolyORB.POA_Config.Proxies;
with PolyORB.POA.Basic_POA;

procedure PolyORB.Setup.Proxies_POA
  (Root_POA_Object :        PolyORB.POA.Obj_Adapter_Access;
   Error           : in out PolyORB.Errors.Error_Container)
is

   use PolyORB.POA_Manager;
   use PolyORB.POA.Basic_POA;

   use type PolyORB.POA.Obj_Adapter_Access;

   Proxies_POA_Configuration : POA_Config.Proxies.Configuration;

   Proxy_POA   : PolyORB.POA.Obj_Adapter_Access;
begin
   if PolyORB.Parameters.Get_Conf ("proxies", "enable_proxies", False) then

      pragma Assert (Root_POA_Object /= null);

      PolyORB.POA.Basic_POA.Create_POA
        (Basic_Obj_Adapter (Root_POA_Object.all)'Access,
         "Proxies",
         POAManager_Access (Entity_Of (Root_POA_Object.POA_Manager)),
         POA_Config.Default_Policies
         (POA_Config.Configuration_Type'Class (Proxies_POA_Configuration)),
         Proxy_POA,
         Error);

      if PolyORB.Errors.Found (Error) then
         return;
      end if;

      PolyORB.POA.Basic_POA.Set_Proxies_OA
        (POA.Basic_POA.Basic_Obj_Adapter_Access (Root_POA_Object),
         POA.Basic_POA.Basic_Obj_Adapter_Access (Proxy_POA));
   end if;

end PolyORB.Setup.Proxies_POA;
