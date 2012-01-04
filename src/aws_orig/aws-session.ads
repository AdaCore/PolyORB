------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          A W S . S E S S I O N                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
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

--  This is the API to handle session data for each client connected.

--  @@@ uses ada.calendar

with Ada.Calendar;

package AWS.Session is

   type ID is private;

   No_Session : constant ID;

   function Create return ID;
   --  Create a new uniq Session ID.

   procedure Delete (SID : ID);
   --  Delete session, does nothing if SID does not exists.

   function Image (SID : ID) return String;
   --  Return ID image

   function Value (SID : String) return ID;
   --  Build an ID from a String, returns No_Session if SID is not recongnized
   --  as an AWS session ID.

   function Exist (SID : ID) return Boolean;
   --  Returns True if SID exist

   procedure Touch (SID : ID);
   --  Update to current time the timestamp associated with SID. Does nothing
   --  if SID does not exists.

   procedure Set
     (SID   : ID;
      Key   : String;
      Value : String);
   --  Set key/pair value for the SID.

   procedure Set
     (SID   : ID;
      Key   : String;
      Value : Integer);
   --  Set key/pair value for the SID.

   procedure Set
     (SID   : ID;
      Key   : String;
      Value : Float);
   --  Set key/pair value for the SID.

   procedure Set
     (SID   : ID;
      Key   : String;
      Value : Boolean);
   --  Set key/pair value for the SID.

   function Get
     (SID : ID;
      Key : String)
     return String;
   --  Returns the Value for Key in the session SID or the emptry string if
   --  key does not exist.

   function Get
     (SID : ID;
      Key : String)
      return Integer;
   --  Returns the Value for Key in the session SID or the integer value 0 if
   --  key does not exist or is not an integer.

   function Get
     (SID : ID;
      Key : String)
      return Float;
   --  Returns the Value for Key in the session SID or the float value 0.0 if
   --  key does not exist or is not a float.

   function Get
     (SID : ID;
      Key : String)
      return Boolean;
   --  Returns the Value for Key in the session SID or the boolean False if
   --  key does not exist or is not a boolean.

   procedure Remove
     (SID : ID;
      Key : String);
   --  Removes Key from the specified session.

   function Exist
     (SID : ID;
      Key : String)
      return Boolean;
   --  Returns True if Key exist in session SID.

   generic
      with procedure Action
        (N          : Positive;
         SID        : ID;
         Time_Stamp : Ada.Calendar.Time;
         Quit       : in out Boolean);
   procedure For_Every_Session;
   --  Iterator which call Action for every active session. N is the SID
   --  order. Time_Stamp is the time when SID was updated for the last
   --  time. Quit is set to False by default, it is possible to control the
   --  iterator termination by setting its value to True.

   generic
      with procedure Action
        (N          : Positive;
         Key, Value : String;
         Quit       : in out Boolean);
   procedure For_Every_Session_Data (SID : ID);
   --  Iterator which returns all the key/value pair defined for session SID.
   --  Quit is set to False by default, it is possible to control the iterator
   --  termination by setting its value to True.

   procedure Set_Lifetime (Seconds : Duration);
   --  Set the lifetime for session data.

   function Get_Lifetime return Duration;
   --  Get current session lifetime for session data.

   procedure Save (File_Name : String);
   --  Save all sessions data into File_Name.

   procedure Load (File_Name : String);
   --  Restore all sessions data from File_Name.

private

   pragma Inline (Image, Value);

   type ID is new String (1 .. 11);

   No_Session : constant ID := (others => ' ');

   task type Cleaner is
      entry Stop;
   end Cleaner;
   --  Call Database.Clean every Session_Lifetime seconds.

   type Cleaner_Access is access Cleaner;

   Cleaner_Task : Cleaner_Access;

   ---------------------
   -- Cleaner_Control --
   ---------------------

   protected Cleaner_Control is

      procedure Start
        (Session_Check_Interval : Duration;
         Session_Lifetime       : Duration);
      --  Launch the cleaner task the first time and does nothing after.

      procedure Stop (Need_Release : out Boolean);
      --  Stop the cleaner task when there is no more server using it. Release
      --  is set to True if the Cleaner_Task can be released.

   private
      Server_Count : Natural := 0;
   end Cleaner_Control;

end AWS.Session;
