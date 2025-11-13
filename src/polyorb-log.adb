------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . L O G                           --
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

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Initialization;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package body PolyORB.Log is

   use PolyORB.Utils.Strings;

   type Log_Level_Ptr is access all Log_Level;

   procedure Output
     (Facility_Level : Log_Level_Ptr;
      Facility       : String;
      Message        : String;
      Level          : Log_Level);
   --  Common code shared by all instances of Facility_Log:
   --  * if Facility_Level is Unknown, look up log level for Facility,
   --    and cache it in Facility_Level if now known;
   --  * if still unknown, buffer message for further processing;
   --  * else output Message if Level >= Facility_Level.

   -------------------
   -- Get_Log_Level --
   -------------------

   function Get_Log_Level (Facility : String) return Log_Level;
   --  Returns the user-requested log level for facility Flag.

   function Get_Log_Level (Facility : String) return Log_Level is
      use type Initialization.Configuration_Hook;
      Level : Log_Level;
   begin
      if Initialization.Get_Conf_Hook /= null then
         declare
            Level_Name : constant String := Initialization.Get_Conf_Hook
                           (Section => Log_Section,
                            Key     => Facility,
                            Default => Log_Level'Image (Default_Log_Level));
         begin
            Level := Log_Level'Value (Level_Name);
            if Level = Unknown then
               Level := Default_Log_Level;
            end if;
         exception
            when others =>
               Level := Default_Log_Level;
         end;
         return Level;
      else
         return Unknown;
      end if;
   end Get_Log_Level;

   -------------------------------
   -- Generic body Facility_Log --
   -------------------------------

   package body Facility_Log is

      Facility_Level : aliased Log_Level := Unknown;

      -------------
      -- Enabled --
      -------------

      function Enabled (Level : Log_Level := Debug) return Boolean is
      begin
         return Facility_Level = Unknown or else Level >= Facility_Level;
      end Enabled;

      ------------
      -- Output --
      ------------

      procedure Output
        (Message : String;
         Level   : Log_Level := Debug) is
      begin

         --  Unchecked_Access needed here because the lifetime of
         --  Facility_Level is that of the Facility_Log instance, and the
         --  compiler has no means of knowing that it is not less than the
         --  lifetime of PolyORB.Log.

         Log.Output
           (Facility_Level'Unchecked_Access, Facility, Message, Level);
      end Output;

   end Facility_Log;

   --------------------------------
   -- Package body for Internals --
   --------------------------------

   package body Internals is

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line (S : String) is
      begin
         if Log_Hook /= null then
            Log_Hook.all (S);
         end if;
      end Put_Line;

   end Internals;

   type Log_Request is record
      Facility_Level : Log_Level_Ptr;
      Facility       : String_Ptr;
      Message        : String_Ptr;
      Level          : Log_Level;
   end record;

   --  During initialization (before the configuration and logging modules
   --  are initialized), messages are captured in a buffer. Once initialization
   --  has been completed, the buffer is flushed.

   package Request_Lists is new PolyORB.Utils.Chained_Lists (Log_Request);
   type Request_List_Access is access Request_Lists.List;
   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Request_Lists.List,
      Name => Request_List_Access);
   Buffer : Request_List_Access;

   Buffer_Enable : Boolean := True;
   --  Buffering is disabled as soon as Initialize is called

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use Request_Lists;
      It : Request_Lists.Iterator;
   begin
      --  Get default log level from configuration

      Default_Log_Level := Get_Log_Level ("default");

      --  No more buffering after this point

      Buffer_Enable := False;

      if Buffer = null then
         return;
      end if;

      It := First (Buffer.all);
      while not Last (It) loop
         declare
            R : Log_Request renames Value (It).all;
         begin
            Output (R.Facility_Level, R.Facility.all, R.Message.all, R.Level);
            Free (R.Facility);
            Free (R.Message);
         end;
         Next (It);
      end loop;
      Deallocate (Buffer.all);
      Free (Buffer);
   end Initialize;

   ------------
   -- Output --
   ------------

   procedure Output
     (Facility_Level : Log_Level_Ptr;
      Facility       : String;
      Message        : String;
      Level          : Log_Level) is
   begin
      if Facility_Level.all = Unknown then
         Facility_Level.all := Get_Log_Level (Facility);
      end if;

      if Buffer_Enable then
         if Buffer = null then
            Buffer := new Request_Lists.List;
         end if;
         Request_Lists.Append (Buffer.all,
           Log_Request'(Facility_Level => Facility_Level,
                        Facility => +Facility,
                        Message  => +Message,
                        Level    => Level));
      elsif Level >= Facility_Level.all then
         Internals.Put_Line (Facility & ": " & Message);
      end if;
   end Output;

end PolyORB.Log;
