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

package System.Garlic.Utils is

   type String_Access is access String;

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

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

   type Action_Type is (Modified, Unmodified, Wait_Until_Modified);

   protected type Semaphore_Type is
      entry Lock;
      entry Unlock (Post : Action_Type := Unmodified);
   private
      entry Wait (Post : Action_Type := Unmodified);
      Locked : Boolean := False;
      Action : Action_Type := Unmodified;
   end Semaphore_Type;

   type Semaphore_Access is access Semaphore_Type;

   procedure Free is
     new Ada.Unchecked_Deallocation (Semaphore_Type, Semaphore_Access);

   procedure Raise_With_Errno (Id : in Ada.Exceptions.Exception_Id);
   pragma Inline (Raise_With_Errno);
   --  Raise an exception with a message corresponding to errno.

   procedure Raise_Communication_Error (Msg : in String := "");
   pragma Inline (Raise_Communication_Error);
   --  Idem, but with the specific exception System.RPC.Communication_Error.
   --  If an alternate message is given, it will be used instead.

   function Different (V1, V2 : String) return Boolean;
   --  Compare two version ids. If one of these version ids is a string
   --  of blank characters then they are identical.

end System.Garlic.Utils;
