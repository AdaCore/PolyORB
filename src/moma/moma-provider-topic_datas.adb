------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            M O M A . P R O V I D E R . T O P I C _ D A T A S             --
--                                                                          --
--                                 B o d y                                  --
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

package body MOMA.Provider.Topic_Datas is

   use MOMA.Types;

   use PolyORB.Tasking.Rw_Locks;

   --------------------
   -- Add_Subscriber --
   --------------------

   procedure Add_Subscriber (Data      : Topic_Data;
                             Topic_Id  : MOMA.Types.String;
                             Pool      : PolyORB.References.Ref)
   is
      V : Topic;
      T : String := To_Standard_String (Topic_Id);
   begin
      Lock_W (Data.T_Lock);
      V := Lookup (Data.T, T);
      Ref_List.Append (V.Subscribers, Pool);
      Unlock_W (Data.T_Lock);
   exception
      when No_Key =>
         Insert (Data.T, T, New_Topic (Ref_List."+" (Pool)));
         Unlock_W (Data.T_Lock);
   end Add_Subscriber;

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization (W : in out Topic_Data)
   is
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

   function Get_Subscribers (Data   : Topic_Data;
                             Topic_Id : MOMA.Types.String)
      return Ref_List.List
   is
      V           : Topic;
      Subscribers : Ref_List.List;
      K           : String := To_Standard_String (Topic_Id);
   begin
      --  XXX Should we call Ensure_Initialization ?
      Lock_R (Data.T_Lock);
      V := Lookup (Data.T, K);
      Subscribers := Ref_List.Duplicate (V.Subscribers);
      Unlock_R (Data.T_Lock);
      return Subscribers;
   exception
      when No_Key =>
         Unlock_R (Data.T_Lock);
         return Subscribers;
   end Get_Subscribers;

   ------------
   -- Lookup --
   ------------

   function Lookup
      (W : Topic_Data;
       K : String)
     return Topic
   is
      Result : Topic;
   begin
      --  XXX Should we call Ensure_Initialization ?
      Lock_R (W.T_Lock);
      Result := Lookup (W.T, K);
      Unlock_R (W.T_Lock);
      return Result;
   exception
         when No_Key => raise Key_Not_Found;
   end Lookup;

   function Lookup
     (W : Topic_Data;
      K : String;
      Default : Topic)
     return Topic
   is
      V : Topic;
   begin
      --  XXX  Should we call Ensure_Initialization ?
      Lock_R (W.T_Lock);
      V := Lookup (W.T, K, Default);
      Unlock_R (W.T_Lock);
      return V;
   end Lookup;

   ---------------
   -- New_Topic --
   ---------------

   function New_Topic (S : Ref_List.List) return Topic
   is
   begin
      return Topic'(To_MOMA_String ("Unknown"), S);
   end New_Topic;

   --------------
   -- Register --
   --------------

   procedure Register
     (W : in out Topic_Data;
      K : String;
      T : Topic)
   is
   begin
      Ensure_Initialization (W);
      Lock_W (W.T_Lock);
      Insert (W.T, K, T);
      Unlock_W (W.T_Lock);
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (W : in out Topic_Data;
      K : String)
   is
   begin
      Ensure_Initialization (W);
      Lock_W (W.T_Lock);
      Delete (W.T, K);
      Unlock_W (W.T_Lock);
   exception
      when No_Key => raise Key_Not_Found;
   end Unregister;

end MOMA.Provider.Topic_Datas;
