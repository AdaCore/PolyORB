------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . M O M A _ P . P R O V I D E R . T O P I C _ D A T A S   --
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

--  A dynamic, protected dictionary of Topics, indexed by Strings.
--  Such a dictionary is used by a router to retrieve topics informations.

with PolyORB.Log;

package body PolyORB.MOMA_P.Provider.Topic_Datas is

   use Perfect_Htable;

   use MOMA.Types;

   use PolyORB.Log;
   use PolyORB.Tasking.Rw_Locks;

   package L is new PolyORB.Log.Facility_Log ("moma.provider.topic_datas");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   --------------------
   -- Add_Subscriber --
   --------------------

   procedure Add_Subscriber
     (Data      : Topic_Data;
      Topic_Id  : MOMA.Types.String;
      Pool      : MOMA.Destinations.Destination)
   is
      V : Topic;
      T : constant String := To_Standard_String (Topic_Id);
   begin
      pragma Debug (C, O ("Adding to topic " & T & " the Pool "
                       & MOMA.Destinations.Image (Pool)));

      Lock_W (Data.T_Lock);
      V := Lookup (Data.T, T, Null_Topic);

      if V /= Null_Topic then
         Destination_List.Append (V.Subscribers, Pool);
      else
         Insert (Data.T, T, New_Topic (Destination_List."+" (Pool)));
      end if;

      Unlock_W (Data.T_Lock);
   end Add_Subscriber;

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization
     (W : in out Topic_Data) is
   begin
      if W.T_Initialized then
         return;
      end if;

      Initialize (W.T);
      PolyORB.Tasking.Rw_Locks.Create (W.T_Lock);
      W.T_Initialized := True;

   end Ensure_Initialization;

   ---------------------
   -- Get_Subscribers --
   ---------------------

   function Get_Subscribers
     (Data      : Topic_Data;
      Topic_Id  : MOMA.Types.String)
     return Destination_List.List
   is
      V           : Topic;
      Subscribers : Destination_List.List;
      K           : constant String := To_Standard_String (Topic_Id);

   begin
      Lock_R (Data.T_Lock);

      V := Lookup (Data.T, K, Null_Topic);
      if V /= Null_Topic then
         Subscribers := Destination_List.Duplicate (V.Subscribers);
      end if;

      Unlock_R (Data.T_Lock);
      return Subscribers;
   end Get_Subscribers;

   ---------------
   -- New_Topic --
   ---------------

   function New_Topic
     (S : Destination_List.List)
     return Topic is
   begin
      return Topic'(To_MOMA_String ("Unknown"), S);
   end New_Topic;

   -----------------------
   -- Remove_Subscriber --
   -----------------------

   procedure Remove_Subscriber
     (Data      : Topic_Data;
      Topic_Id  : MOMA.Types.String;
      Pool      : MOMA.Destinations.Destination)
   is
      use Destination_List;

      V     : Topic;
      T     : constant String := To_Standard_String (Topic_Id);
   begin
      pragma Debug (C, O ("Removing from topic " & T & " the Pool "
                       & MOMA.Destinations.Image (Pool)));

      Lock_W (Data.T_Lock);
      V := Lookup (Data.T, T, Null_Topic);

      if V = Null_Topic then
         raise Key_Not_Found;
         --  XXX do we really need to raise an exception ?
      end if;

      Destination_List.Remove_Occurrences (V.Subscribers, Pool);
      if V.Subscribers = Destination_List.Empty then
         Delete (Data.T, T);
      end if;

      Unlock_W (Data.T_Lock);
   end Remove_Subscriber;

end PolyORB.MOMA_P.Provider.Topic_Datas;
