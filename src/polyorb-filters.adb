------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . F I L T E R S                       --
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

with PolyORB.Filters.Iface;
with PolyORB.Log;

package body PolyORB.Filters is

   use PolyORB.Components;
   use PolyORB.Filters.Iface;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.filters");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   -------------------
   -- Connect_Lower --
   -------------------

   procedure Connect_Lower (F : access Filter; Lower : Component_Access) is
   begin
      Connect (F.Lower, Lower);
   end Connect_Lower;

   -----------
   -- Lower --
   -----------

   function Lower (F : access Filter) return Component_Access is
   begin
      return F.Lower;
   end Lower;

   -----------
   -- Upper --
   -----------

   function Upper (F : access Filter) return Component_Access is
   begin
      return F.Upper;
   end Upper;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (F : in out Filter) is
   begin
      if F.Upper /= null then
         pragma Debug
           (C, O ("Destroying upper of type "
               & Ada.Tags.External_Tag (F.Upper'Tag)));
         PolyORB.Components.Destroy (F.Upper);
      end if;
   end Destroy;

   -------------------------
   -- Create_Filter_Chain --
   -------------------------

   procedure Create_Filter_Chain
     (Factories :     Factory_Array;
      Bottom    : out Filter_Access;
      Top       : out Filter_Access)
   is
      Lower_F, F : Filter_Access;
   begin
      for J in Factories'Range loop
         Create (Fact => Factories (J), Filt => F);

         pragma Debug (C, O ("Created filter of type "
                          & Ada.Tags.External_Tag (F'Tag)));

         Connect_Lower (F, Component_Access (Lower_F));

         if Lower_F /= null then
            Connect (Lower_F.Upper, Component_Access (F));
         else
            Bottom := F;
         end if;

         Lower_F := F;
      end loop;

      Top := F;
   end Create_Filter_Chain;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (F   : not null access Filter;
      Msg : Message'Class) return Components.Message'Class
   is
   begin
      --  Implement default progagation behaviour

      if False
        or else Msg in Data_Indication'Class
        or else Msg in Connect_Indication'Class
        or else Msg in Connect_Confirmation'Class
        or else Msg in Disconnect_Indication'Class
        or else Msg in Set_Server'Class
      then
         return Emit (F.Upper, Msg);

      elsif False
        or else Msg in Data_Expected'Class
        or else Msg in Data_Out'Class
        or else Msg in Disconnect_Request'Class
        or else Msg in Check_Validity'Class
      then
         return Emit (F.Lower, Msg);

      else
         raise Program_Error;
      end if;
   end Handle_Message;

end PolyORB.Filters;
