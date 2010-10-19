------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . M I N I M A L _ S E R V A N T               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2010, Free Software Foundation, Inc.          --
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

with PolyORB.Errors;

package body PolyORB.Minimal_Servant is

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
     (Self : not null access Implementation;
      Req  : Requests.Request_Access) return Boolean is
   begin
      return Execute_Servant (Self.As_Servant, Req);
   end Execute_Servant;

   function Execute_Servant
     (Self : not null access Servant;
      Req  : Requests.Request_Access) return Boolean
   is
      use PolyORB.Errors;
      use PolyORB.Requests;

      Error : Error_Container;
   begin
      Invoke (Servant'Class (Self.all)'Access, Req);
      Set_Out_Args (Req, Error);
      return True;
   end Execute_Servant;

   ------------------------
   -- To_PolyORB_Servant --
   ------------------------

   function To_PolyORB_Servant (S : access Servant)
     return PolyORB.Servants.Servant_Access is
   begin
      return S.Neutral_View'Access;
   end To_PolyORB_Servant;

end PolyORB.Minimal_Servant;
