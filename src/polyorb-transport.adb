------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . T R A N S P O R T                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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

--  Abstract transport service access points and
--  communication endpoints.

with PolyORB.ORB.Interface;

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

   function Handle_Message
     (TAP : access Transport_Access_Point;
      Msg :        Components.Message'Class)
     return Components.Message'Class is
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

   -----------
   -- Close --
   -----------

   procedure Close (TE : access Transport_Endpoint) is
   begin
      if TE.Closed then
         return;
      end if;
      Emit_No_Reply
        (TE.Server, ORB.Interface.Unregister_Endpoint'
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

   procedure Destroy
     (TE : in out Transport_Endpoint)
   is
   begin
      Annotations.Destroy (TE.Notepad);
      Destroy (TE.Upper);
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
