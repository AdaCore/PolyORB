------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . S E T U P . P R O X I E S _ P O A              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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
