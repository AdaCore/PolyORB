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
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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

with Ada.Unchecked_Deallocation;
with Ada.Task_Identification;

package System.Garlic.Utils is

   pragma Elaborate_Body;

   type String_Access is access String;
   function String_To_Access (S : String) return String_Access;
   function Access_To_String (S : String_Access) return String;
   pragma Stream_Convert (Entity => String_Access,
                          Read   => String_To_Access,
                          Write  => Access_To_String);
   --  Stream attributes

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Access);
   --  Access on string and deallocation procedure. This access type can
   --  be transmitted accross partitions.

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

   type Mutex_Access is private;

   function Create return Mutex_Access;
   --  Allocate a mutex

   procedure Enter (M : in Mutex_Access);
   pragma Inline (Enter);
   --  Enter one level of critical section

   procedure Destroy (M : in out Mutex_Access);
   --  Free the memory used by a Mutex_Access

   procedure Leave (M : in Mutex_Access);
   pragma Inline (Leave);
   --  Leave one level of critical section

   type Version_Id is mod 2 ** 8;

   type Watcher_Access is private;

   procedure Commit (W : in Watcher_Access; V : out Version_Id);
   pragma Inline (Commit);
   --  Fetch W stamp

   function Create return Watcher_Access;
   pragma Inline (Create);
   --  Allocate a watcher

   procedure Destroy (W : in out Watcher_Access);
   pragma Inline (Destroy);
   --  Destroy a watcher

   procedure Differ (W : in Watcher_Access; V : in Version_Id);
   pragma Inline (Differ);
   --  Await until T stamp differs from S

   procedure Update (W : in Watcher_Access);
   pragma Inline (Update);
   --  Increment stamp in W

   type Adv_Mutex_Access is private;

   function Create return Adv_Mutex_Access;
   --  Allocate an advances mutex

   procedure Enter (M : in Adv_Mutex_Access);
   pragma Inline (Enter);
   --  Enter one level of critical section

   procedure Destroy (M : in out Adv_Mutex_Access);
   --  Free the memory used by an Adv_Mutex

   procedure Leave (M : in Adv_Mutex_Access);
   pragma Inline (Leave);
   --  Leave one level of critical section

private

   type Watcher_Type;

   type Watcher_Access is access Watcher_Type;

   type Mutex_Type;

   type Mutex_Access is access Mutex_Type;

   type Adv_Mutex_Type;

   type Adv_Mutex_Access is access Adv_Mutex_Type;

end System.Garlic.Utils;
