------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . F I L T E R S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  A communication filter (a transport Data_Unit handler/forwarder).

--  $Id$

with Ada.Tags;

with PolyORB.Filters.Interface;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

package body PolyORB.Filters is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.filters");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Connect_Lower (F : access Filter; Lower : Component_Access) is
   begin
      Connect (F.Lower, Lower);
   end Connect_Lower;

   function Lower (F : access Filter) return Component_Access is
   begin
      return F.Lower;
   end Lower;

   function Upper (F : access Filter) return Component_Access is
   begin
      return F.Upper;
   end Upper;

   procedure Finalize (F : in out Filter) is
   begin
      if F.Upper /= null then
         pragma Debug
           (O ("Destroying upper of type "
               & Ada.Tags.External_Tag (F.Upper'Tag)));
         Destroy (F.Upper);
         --  XXX WHAT IF F.Upper has not been dynamically allocated?
      end if;
   end Finalize;

   function Handle_Message
     (F : access Factory;
      Msg : Message'Class)
     return Message'Class is
   begin
      if Msg in Interface.Create_Filter_Chain then
         return Interface.Created_Filter_Chain'
           (Filter_Chain => Create_Filter_Chain (F));
      else
         raise Unhandled_Message;
      end if;
   end Handle_Message;

   procedure Chain_Factories (Factories : Factory_Array) is
   begin
      for I in Factories'First .. Factories'Last - 1 loop
         pragma Debug
           (O ("Chaining "
               & Ada.Tags.External_Tag (Factories (I)'Tag)
               & " to "
               & Ada.Tags.External_Tag (Factories (I + 1)'Tag)));
         Connect
           (Factories (I).Upper,
            Component_Access (Factories (I + 1)));
      end loop;

      Factories (Factories'Last).Upper := null;
   end Chain_Factories;

   function Create_Filter_Chain (FChain : access Factory)
     return Filter_Access
   is
      F : Filter_Access;
   begin
      Create (Fact => Factory'Class (FChain.all)'Access, Filt => F);
      pragma Debug (O ("Created filter of type "
                       & Ada.Tags.External_Tag (F'Tag)));
      --  Create new filter.

      if FChain.Upper /= null then
         declare
            Reply : constant Message'Class
              := Emit
              (FChain.Upper,
               Interface.Create_Filter_Chain'(null record));
         begin
            if not (Reply in Interface.Created_Filter_Chain) then
               raise Unhandled_Message;
            end if;

            declare
               Upper : constant Filter_Access
                 := Interface.Created_Filter_Chain
                 (Reply).Filter_Chain;
            begin
               Connect (F.Upper, Component_Access (Upper));
               Connect_Lower (Upper, Component_Access (F));
            end;
         end;
      end if;
      return F;
   end Create_Filter_Chain;

end PolyORB.Filters;
