------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                     G L A D E . S E M A P H O R E S                      --
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

with Ada.Streams;

package GLADE.Semaphores is

   pragma Remote_Types;

   type Semaphore_Type is tagged limited private;
   --  Semaphore

   procedure Initialize
     (Semaphore : access Semaphore_Type;
      Name      : in String);
   --  Initialize a semaphore given its name. The same name must be used
   --  to have several virtual instances of the semaphore, which is quite
   --  useful.

   procedure P (Semaphore : access Semaphore_Type);
   --  Get the semaphore. This is a blocking operation since the semaphore
   --  may be used by someone else at this time. Program_Error is raised if
   --  the semaphore has not been initialized.

   procedure V (Semaphore : access Semaphore_Type);
   --  Release the semaphore. Program_Error is raised if no P operation
   --  has been performed on the semaphore.

   function Acquired (Semaphore : access Semaphore_Type) return Boolean;
   pragma Inline (Acquired);
   --  Return True if the semaphore has been acquired at the time of the call

   function Semaphore_Name (Semaphore : access Semaphore_Type)
     return String;
   --  Get the name that has been used to initialize a semaphore

   function Semaphore_Index (Semaphore : access Semaphore_Type)
     return Positive;
   --  Return an index that is unique for each semaphore

   function Semaphore_Partition (Semaphore : access Semaphore_Type)
     return Natural;
   --  Return a partition on which Semaphore is located

   type Semaphore_Access is access all Semaphore_Type'Class;

private

   procedure Send_Request_To (Recipient : access Semaphore_Type;
                              Client    : in Semaphore_Access);
   --  Send the request for the semaphore

   procedure Signal (Recipient : access Semaphore_Type);
   --  The semaphore can be used

   type String_Access is access String;
   --  The type from GLADE.Types cannot be used because of the categorization
   --  of this package.

   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    Str    : in String_Access);

   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   Str    : out String_Access);

   for String_Access'Write use Write;
   for String_Access'Read use Read;

   type Status_Type is (Locked, Waiting, Unlocked);

   type Semaphore_Type is tagged limited record
      Status         : Status_Type;
      Probable_Owner : Semaphore_Access;
      Promised_To    : Semaphore_Access;
      Barrier        : Positive;
      Name           : String_Access;
      Index          : Positive;
   end record;

end GLADE.Semaphores;
