------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            M O M A . P R O V I D E R . T O P I C _ D A T A S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

--  A dynamic, protected dictionary of Topics, indexed by Strings.
--  Such a dictionary is used by a router to retrieve topics informations.

--  $Id$

with PolyORB.References;
with PolyORB.Tasking.Rw_Locks;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.HTables.Perfect;

with MOMA.Types;

package MOMA.Provider.Topic_Datas is

   package Ref_List is
      new PolyORB.Utils.Chained_Lists (PolyORB.References.Ref);

   type Topic is record
      Name        : MOMA.Types.String;
      Subscribers : Ref_List.List;
   end record;
   --  Name : Name of the topic.
   --  Subscribers   : chained list of references, which are the references of
   --                  the message pools subscribed to this topic.
   --  XXX Should be private.

   package Perfect_Htable is
      new PolyORB.Utils.HTables.Perfect (Topic);

   use Perfect_Htable;

   Key_Not_Found : exception;

   type Topic_Data is private;

   procedure Add_Subscriber (Data      : Topic_Data;
                             Topic_Id  : MOMA.Types.String;
                             Pool      : PolyORB.References.Ref);
   --  Add a new pool in the subsribers list of a topic.

   procedure Ensure_Initialization (W : in out Topic_Data);
   --  Ensure that T was initialized.

   function Get_Subscribers (Data      : Topic_Data;
                             Topic_Id  : MOMA.Types.String)
      return Ref_List.List;
   --  Return the list of current subscribers to a given topic.

private

   type Topic_Data is record
      T             : Table_Instance;
      T_Initialized : Boolean := False;
      T_Lock        : PolyORB.Tasking.Rw_Locks.Rw_Lock_Access;
   end record;

   procedure Register
     (W : in out Topic_Data;
      K : String;
      T : Topic);
   --  Associate key K with topic T.

   procedure Unregister
     (W : in out Topic_Data;
      K : String);
   --  Remove any association for K. Key_Not_Found is raised
   --  if no topic was registered for this key.

   function Lookup
      (W : Topic_Data;
       K : String)
     return Topic;
   --  Lookup K in the dictionary, and return the associated topic.
   --  Key_Not_Found is raised if no topic was registered for this key.

   function Lookup
     (W : Topic_Data;
      K : String;
      Default : Topic)
     return Topic;
   --  As above, but Default is returned for non-registered keys,
   --  instead of raising an exception.

   function New_Topic (S : Ref_List.List) return Topic;
   --  Return a new topic with the list of subscribers S.

end MOMA.Provider.Topic_Datas;
