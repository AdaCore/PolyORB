------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . U T I L S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Task_Identification;

package System.Garlic.Utils is

   pragma Elaborate_Body;

   type String_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);
   --  String access type used throughout Garlic

   procedure To_Lower (Item : in out String);
   pragma Inline (To_Lower);
   --  In place transformation of a string with all the upper-case letters
   --  changed into corresponding lower-case ones.

   protected type Barrier_Type is
      entry Wait;
      procedure Signal (How_Many : Positive := 1);
      procedure Signal_All (Permanent : Boolean);
   private
      Free : Natural := 0;
      Perm : Boolean := False;
   end Barrier_Type;
   --  Any number of task may be waiting on Wait. Signal unblocks How_Many
   --  tasks (the order depends on the queuing policy) and Signal_All unblocks
   --  all the tasks (if Permanent is True, Wait will no longer be blocking).
   --  If How_Many is more than the number of tasks waiting, new tasks will
   --  be awakened as well.

   type Barrier_Access is access Barrier_Type;
   procedure Free is
     new Ada.Unchecked_Deallocation (Barrier_Type, Barrier_Access);

   type Status_Type is (Modified, Unmodified, Postponed);

   protected type Mutex_Type is
      entry Enter;
      entry Leave (S : Status_Type := Unmodified);
   private
      entry Wait (S : Status_Type);
      Locked : Boolean := False;
      Status : Status_Type := Unmodified;
   end Mutex_Type;
   --  A task can enter in a mutex if and only if there is no other task in
   --  a mutex. A task can leave a mutex returning the status of its
   --  action.  Unmodified corresponds to no modification of the protected
   --  data. Postponed indicates that the task would like to be resumed
   --  when the protected data is modified (in order to enter the mutex
   --  another time). Modified corresponds to a modification and resume
   --  all the tasks pending for a modification of the protected data.

   type Mutex_Access is access Mutex_Type;
   procedure Free is
     new Ada.Unchecked_Deallocation (Mutex_Type, Mutex_Access);

   type Adv_Mutex_Type is record
      Mutex     : Mutex_Access;
      Current   : Ada.Task_Identification.Task_Id;
      Level     : Natural;
   end record;
   --  This is a classical critical section except that when a task try to
   --  Enter a critical section several times without leaving it first it
   --  is not blocked and can continue. Leave keeps track of the number of
   --  times Enter has been successful.

   type Adv_Mutex_Access is access Adv_Mutex_Type;
   function Allocate return Adv_Mutex_Access;

   procedure Enter (M : Adv_Mutex_Access);
   pragma Inline (Enter);
   --  Enter one level of critical section

   procedure Free (M : in out Adv_Mutex_Access);
   --  Free the memory used by an Adv_Mutex

   procedure Leave (M : Adv_Mutex_Access; S : Status_Type := Unmodified);
   pragma Inline (Leave);
   --  Leave one level of critical section. S is used by inner mutex when
   --  leaving first level of critical section.

   Global_Mutex : Adv_Mutex_Access;
   --  Global mutex to be used for coarse grained locking. Justified in
   --  every case ???

   procedure Raise_With_Errno (Id : in Ada.Exceptions.Exception_Id);
   pragma Inline (Raise_With_Errno);
   --  Raise an exception with a message corresponding to errno

   procedure Raise_Communication_Error (Msg : in String := "");
   pragma Inline (Raise_Communication_Error);
   --  Idem, but with the specific exception System.RPC.Communication_Error.
   --  If an alternate message is given, it will be used instead.

   function Different (V1, V2 : String) return Boolean;
   --  Compare two version ids. If one of these version ids is a string
   --  of blank characters then they will be considered as identical.

end System.Garlic.Utils;
