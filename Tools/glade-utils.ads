------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                          G L A D E . U T I L S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
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

package GLADE.Utils is

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

   type Barrier_Type is limited private;

   procedure Create (B : out Barrier_Type);
   pragma Inline (Create);
   --  Allocate a barrier.

   procedure Destroy (B : in out Barrier_Type);
   pragma Inline (Destroy);
   --  Destroy a barrier.

   procedure Signal (B : in Barrier_Type; N : in Positive := 1);
   pragma Inline (Signal);
   --  Release N processes waiting on the barrier.

   procedure Signal_All (B : in Barrier_Type; P : in Boolean := True);
   pragma Inline (Signal_All);
   --  Release all processes waiting on the barrier. If P is true, this
   --  barrier is no longer blocking.

   procedure Wait (B : in Barrier_Type);
   pragma Inline (Wait);
   --  Wait on barrier.

   type Mutex_Type is limited private;

   procedure Create (M : out Mutex_Type);
   --  Allocate a mutex

   procedure Enter (M : in Mutex_Type);
   pragma Inline (Enter);
   --  Enter one level of critical section

   procedure Destroy (M : in out Mutex_Type);
   --  Free the memory used by a Mutex_Type

   procedure Leave (M : in Mutex_Type);
   pragma Inline (Leave);
   --  Leave one level of critical section

   type Version_Id is mod 2 ** 8;
   No_Version : constant Version_Id := 0;

   type Watcher_Type is limited private;

   procedure Lookup (W : in Watcher_Type; V : out Version_Id);
   pragma Inline (Lookup);
   --  Fetch W stamp

   procedure Create (W : out Watcher_Type);
   pragma Inline (Create);
   --  Allocate a watcher

   procedure Destroy (W : in out Watcher_Type);
   pragma Inline (Destroy);
   --  Destroy a watcher

   procedure Differ (W : in Watcher_Type; V : in Version_Id);
   pragma Inline (Differ);
   --  Await until T stamp differs from S

   procedure Update (W : in Watcher_Type);
   pragma Inline (Update);
   --  Increment stamp in W

   type Adv_Mutex_Type is private;

   procedure Create (M : out Adv_Mutex_Type);
   --  Allocate an advances mutex

   procedure Enter (M : in Adv_Mutex_Type);
   pragma Inline (Enter);
   --  Enter one level of critical section

   procedure Destroy (M : in out Adv_Mutex_Type);
   --  Free the memory used by an Adv_Mutex

   procedure Leave (M : in Adv_Mutex_Type);
   pragma Inline (Leave);
   --  Leave one level of critical section

private

   type Barrier_PO;

   type Barrier_Type is access Barrier_PO;

   type Watcher_PO;

   type Watcher_Type is access Watcher_PO;

   type Mutex_PO;

   type Mutex_Type is access Mutex_PO;

   type Adv_Mutex_PO;

   type Adv_Mutex_Type is access Adv_Mutex_PO;

end GLADE.Utils;
