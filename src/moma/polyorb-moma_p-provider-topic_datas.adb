------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . M O M A _ P . P R O V I D E R . T O P I C _ D A T A S   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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

      Destination_List.Remove (V.Subscribers, Pool);
      if V.Subscribers = Destination_List.Empty then
         Delete (Data.T, T);
      end if;

      Unlock_W (Data.T_Lock);
   end Remove_Subscriber;

end PolyORB.MOMA_P.Provider.Topic_Datas;
