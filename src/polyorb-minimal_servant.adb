------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . M I N I M A L _ S E R V A N T               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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

with Ada.Tags;

with PolyORB.Log;
with PolyORB.Servants.Iface;
with PolyORB.Errors;

package body PolyORB.Minimal_Servant is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.minimal_servant");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
     (Self : not null access Implementation;
      Msg  : Components.Message'Class) return Components.Message'Class is
   begin
      return Execute_Servant (Self.As_Servant, Msg);
   end Execute_Servant;

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
     (Self : not null access Servant;
      Msg  : Components.Message'Class) return Components.Message'Class
   is
      use PolyORB.Servants.Iface;

   begin
      pragma Debug (C, O ("Handling message of type "
                & Ada.Tags.External_Tag (Msg'Tag)));

      if Msg in Execute_Request then
         declare
            use PolyORB.Requests;
            use PolyORB.Errors;

            R : constant Request_Access := Execute_Request (Msg).Req;
            Error : Error_Container;
         begin
            Invoke (Servant'Class (Self.all)'Access, R);

            Set_Out_Args (R, Error);
            --  XXX We should do something if we find an exception

            return Executed_Request'(Req => R);
         end;
      else
         raise Program_Error;
      end if;
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
