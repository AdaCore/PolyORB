------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . F I L T E R S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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

--  A communication filter (a transport Data_Unit handler/forwarder).

with Ada.Tags;

with PolyORB.Log;

package body PolyORB.Filters is

   use PolyORB.Components;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.filters");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

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
           (O ("Destroying upper of type "
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
         Set_Allocation_Class (F.all, Dynamic);
         pragma Debug (O ("Created filter of type "
                          & Ada.Tags.External_Tag (F'Tag)));
         --  Create new filter.

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

end PolyORB.Filters;
