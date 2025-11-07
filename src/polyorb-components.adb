------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . C O M P O N E N T S                    --
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

--  A communication filter (a transport Data_Unit handler/forwarder).

with Ada.Tags;
pragma Warnings (Off, Ada.Tags);
--  Only used within pragma Debug.
with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Log;

package body PolyORB.Components is

   use Ada.Tags;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.components");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Port   : out Component_Access;
      Target :     Component_Access) is
   begin
      Port := Target;
   end Connect;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Comp : in out Component) is
      pragma Unreferenced (Comp);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Comp : in out Component_Access) is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Component'Class,
         Name => Component_Access);
   begin
      pragma Debug (C, O ("Destroying component "
        & Ada.Tags.External_Tag (Comp'Tag)));

      Destroy (Comp.all);
      Free (Comp);
   end Destroy;

   ----------
   -- Emit --
   ----------

   function Emit
     (Port : Component_Access;
      Msg  : Message'Class)
     return Message'Class
   is
      Res : constant Null_Message := (null record);
   begin
      if Port /= null then
         pragma Debug
           (C, O ("Sending message " & External_Tag (Msg'Tag)
               & " to target " & External_Tag (Port.all'Tag)));
         return Handle_Message (Port, Msg);
      else
         pragma Debug
           (C, O ("Message " & External_Tag (Msg'Tag)
               & " ignored (null target)"));
         return Res;
      end if;
   end Emit;

   -------------------
   -- Emit_No_Reply --
   -------------------

   procedure Emit_No_Reply
     (Port : Component_Access;
      Msg  : Message'Class)
   is
      Reply : constant Message'Class := Emit (Port, Msg);
   begin
      pragma Assert (Reply in Null_Message);
      null;
   end Emit_No_Reply;

end PolyORB.Components;
