------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           C O R B A . I M P L                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

package body CORBA.Impl is

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
     (Self : not null access Implementation;
      Req  : PolyORB.Requests.Request_Access) return Boolean
   is
   begin
      return Execute_Servant (Self.As_Object, Req);
   end Execute_Servant;

   function Execute_Servant
     (Self : not null access Object;
      Req  : PolyORB.Requests.Request_Access) return Boolean
   is
   begin
      raise Program_Error;
      return False;
   end Execute_Servant;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (O : in out Object) is
   begin
      Destroy (O.Neutral_View);
   end Finalize;

   ------------------------
   -- To_PolyORB_Servant --
   ------------------------

   function To_PolyORB_Servant
     (S : access Object) return PolyORB.Servants.Servant_Access
   is
   begin
      return S.Neutral_View'Access;
   end To_PolyORB_Servant;

   package body Internals is

      ----------------------
      -- To_CORBA_Servant --
      ----------------------

      function To_CORBA_Servant
        (S : PolyORB.Servants.Servant_Access)
        return Object_Ptr
      is
         use type PolyORB.Servants.Servant_Access;

      begin
         if S = null then
            return null;
         else
            return Object_Ptr (Implementation (S.all).As_Object);
         end if;
      end To_CORBA_Servant;

   end Internals;

end CORBA.Impl;
