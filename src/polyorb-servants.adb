------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O L Y O R B . S E R V A N T S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Objects.Interface;
with PolyORB.Log;

package body PolyORB.Servants is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.servants");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ----------------
   -- Notepad_Of --
   ----------------

   function Notepad_Of
     (S : Servant_Access)
     return PolyORB.Annotations.Notepad_Access is
   begin
      return S.Notepad'Access;
   end Notepad_Of;

   -----------------------
   -- Set_Thread_Policy --
   -----------------------

   procedure Set_Thread_Policy
     (S  : access Servant;
      TP :        POA_Policies.Thread_Policy.ThreadPolicy_Access) is
   begin
      S.TP_Access := TP;
   end Set_Thread_Policy;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (S   : access Servant;
      Msg :        Components.Message'Class)
      return Components.Message'Class
   is
      use PolyORB.Objects.Interface;
      use PolyORB.POA_Policies.Thread_Policy;

   begin
      if Msg in Execute_Request then

         if S.TP_Access = null then
            O ("No thread policy specified for servant");
            raise Program_Error;
         end if;

         --  Dispatch by OA thread policy
         pragma Debug (O ("POA Thread policy is "
                          & Policy_Id (S.TP_Access.all)));
         return Handle_Request_Execution
           (S.TP_Access,
            Msg,
            PolyORB.Components.Component_Access (S));

      else
         pragma Debug (O ("Message not in Execute_Request"));
         raise PolyORB.Components.Unhandled_Message;
      end if;
   end Handle_Message;

end PolyORB.Servants;
