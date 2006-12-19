------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . M O M A _ P . P R O V I D E R . T O P I C _ D A T A S   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Tasking.Rw_Locks;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.HFunctions.Hyper;
with PolyORB.Utils.HTables.Perfect;

with MOMA.Destinations;
with MOMA.Types;
pragma Elaborate_All (MOMA.Types);

package PolyORB.MOMA_P.Provider.Topic_Datas is

   use MOMA.Destinations;

   package Destination_List is
      new PolyORB.Utils.Chained_Lists (MOMA.Destinations.Destination,
                                       MOMA.Destinations."=");
   --  A chained list of destinations.

   type Topic is private;
   --  Name          : Name of the topic.
   --  Subscribers   : chained list of destinations, which are the message
   --                  pools subscribed to this topic.
   --  XXX Maybe not necessary to store a name...

   Null_Topic : constant Topic;

   Key_Not_Found : exception;

   type Topic_Data is private;

   procedure Add_Subscriber
     (Data      : Topic_Data;
      Topic_Id  : MOMA.Types.String;
      Pool      : MOMA.Destinations.Destination);
   --  Add a new pool in the subscribers list of a topic.

   procedure Ensure_Initialization (W : in out Topic_Data);
   --  Ensure that T was initialized.

   procedure Remove_Subscriber
     (Data      : Topic_Data;
      Topic_Id  : MOMA.Types.String;
      Pool      : MOMA.Destinations.Destination);
   --  Remove a pool from the subscribers list of a topic.

   function Get_Subscribers
     (Data      : Topic_Data;
      Topic_Id  : MOMA.Types.String)
     return Destination_List.List;
   --  Return the list of current subscribers to a given topic.

private

   type Topic is record
      Name        : MOMA.Types.String;
      Subscribers : Destination_List.List;
   end record;

   Null_Topic : constant Topic := (Name => MOMA.Types.To_MOMA_String (""),
                                   Subscribers => Destination_List.Empty);

   package Perfect_Htable is
      new PolyORB.Utils.HTables.Perfect
     (Topic,
      PolyORB.Utils.HFunctions.Hyper.Hash_Hyper_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Default_Hash_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Hash,
      PolyORB.Utils.HFunctions.Hyper.Next_Hash_Parameters);

   type Topic_Data is record
      T             : Perfect_Htable.Table_Instance;
      T_Initialized : Boolean := False;
      T_Lock        : PolyORB.Tasking.Rw_Locks.Rw_Lock_Access;
   end record;

   function New_Topic (S : Destination_List.List) return Topic;
   --  Return a new topic with the list of subscribers S.

end PolyORB.MOMA_P.Provider.Topic_Datas;
