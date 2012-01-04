------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . M I N I M A L _ S E R V A N T               --
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
   exception
      when Discard_Request =>
         --  Request is dropped entirely, clear flags to prevent emission of
         --  a reply, and omit Set_Out_Args.

         Req.Req_Flags := Sync_None;
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
