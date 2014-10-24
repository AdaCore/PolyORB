------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . T R A N S P O R T                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2013, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

--  Abstract transport service access points and communication endpoints

with PolyORB.Filters.Iface;
with PolyORB.ORB.Iface;

package body PolyORB.Transport is

   use PolyORB.Components;

   ----------------
   -- Notepad_Of --
   ----------------

   function Notepad_Of
     (TAP : Transport_Access_Point_Access)
     return Annotations.Notepad_Access is
   begin
      return TAP.Notepad'Access;
   end Notepad_Of;

   -------------------
   -- Handle_Mesage --
   -------------------

   overriding function Handle_Message
     (TAP : not null access Transport_Access_Point;
      Msg : Components.Message'Class) return Components.Message'Class
   is
   begin
      raise Program_Error;
      --  Small is beautiful.

      pragma Warnings (Off);
      --  Recent GNAT versions emit a warning for possible
      --  infinite recursion here.

      return Handle_Message (TAP, Msg);
      --  Keep the compiler happy.

      pragma Warnings (On);
   end Handle_Message;

   overriding function Handle_Message
     (TE  : not null access Transport_Endpoint;
      Msg : Components.Message'Class) return Components.Message'Class
   is
      use Filters.Iface;
   begin
      if Msg in Filters.Iface.Check_Validity then
         if not TE.Closed then
            --  If TE is not closed yet, check that it is still valid, which
            --  may cause it to close. Note: under ORB policies with tasking,
            --  this is a no-op as we constantly monitor TEs for I/O. When
            --  there is no tasking on the other hand, a TE might become
            --  invalid undetected, as the partition may be busy executing
            --  application code, so a new check is forced in this case.

            Emit_No_Reply
              (TE.Server, ORB.Iface.Validate_Endpoint'
                            (TE => Transport_Endpoint_Access (TE)));
         end if;

         if TE.Closed then
            declare
               use Errors;
               Reply : Filter_Error;
            begin
               Throw (Reply.Error, Comm_Failure_E,
                 System_Exception_Members'
                  (Minor => 0, Completed => Completed_No));
               return Reply;
            end;
         else
            declare
               Reply : Components.Null_Message;
            begin
               return Reply;
            end;
         end if;

      elsif False
        or else Msg in Connect_Indication
        or else Msg in Connect_Confirmation
      then
         return Emit (TE.Upper, Msg);

      else
         raise Program_Error;
      end if;
   end Handle_Message;

   --------------------
   -- Check_Validity --
   --------------------

   procedure Check_Validity (TE : access Transport_Endpoint) is
   begin
      null;
   end Check_Validity;

   -----------
   -- Close --
   -----------

   procedure Close (TE : access Transport_Endpoint) is
   begin
      if TE.Closed then
         return;
      end if;
      Emit_No_Reply
        (TE.Server, ORB.Iface.Unregister_Endpoint'
                      (TE => Transport_Endpoint_Access (TE)));
      TE.Closed := True;
   end Close;

   -------------------
   -- Connect_Upper --
   -------------------

   procedure Connect_Upper
     (TE    : access Transport_Endpoint;
      Upper :        Components.Component_Access) is
   begin
      Components.Connect (TE.Upper, Upper);
   end Connect_Upper;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (TE : in out Transport_Endpoint)
   is
   begin
      Annotations.Destroy (TE.Notepad);
      Destroy (TE.Upper);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (TE : in out Transport_Endpoint_Access) is
   begin
      Components.Destroy (Components.Component_Access (TE));
   end Destroy;

   ----------------
   -- Notepad_Of --
   ----------------

   function Notepad_Of
     (TE : Transport_Endpoint_Access)
     return Annotations.Notepad_Access is
   begin
      return TE.Notepad'Access;
   end Notepad_Of;

   -----------
   -- Upper --
   -----------

   function Upper
     (TE : Transport_Endpoint_Access)
     return Components.Component_Access is
   begin
      return TE.Upper;
   end Upper;

end PolyORB.Transport;
