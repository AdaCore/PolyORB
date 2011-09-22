------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . D S A _ P . I N I T I A L I Z A T I O N          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2011, Free Software Foundation, Inc.             --
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

with PolyORB.DSA_P.Exceptions;
with PolyORB.Errors;
with PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA_Config;
with PolyORB.POA_Config.RACWs;
with PolyORB.POA_Manager;
with PolyORB.Setup;

package body PolyORB.DSA_P.Initialization is

   ---------------------------------
   -- Initiate_Well_Known_Service --
   ---------------------------------

   procedure Initiate_Well_Known_Service
     (S    : PolyORB.Servants.Servant_Access;
      Name : String)
   is
      use PolyORB.Errors;
      use PolyORB.ORB;
      use PolyORB.POA;
      use PolyORB.POA_Config;
      use PolyORB.POA_Config.RACWs;
      use PolyORB.POA_Manager;

      POA   : Obj_Adapter_Access;
      Error : Error_Container;
   begin
      Create_POA
        (Self         => Obj_Adapter_Access
                           (Object_Adapter (PolyORB.Setup.The_ORB)),
         Adapter_Name => Name,
         A_POAManager => null,
         Policies     => Default_Policies (RACW_POA_Config.all),
         POA          => POA,
         Error        => Error);

      if Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;

      POA.Default_Servant := S;

      Activate (POAManager_Access (Entity_Of (POA.POA_Manager)), Error);

      if Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Initiate_Well_Known_Service;

end PolyORB.DSA_P.Initialization;
