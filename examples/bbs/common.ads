------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C O M M O N                                --
--                                                                          --
--                                 S p e c                                  --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;

package Common is

   pragma Remote_Types;

   type Penpal_Type is tagged limited private;
   --  One particular person

   procedure Initialize
     (Penpal : in out Penpal_Type;
      Name   : String);
   --  Initialize a Penpal name. This will raise Sender_Error if the
   --  Name is empty. You must register this penpal to get new incoming
   --  messages.

   function Name_Of (Penpal : access Penpal_Type) return String;
   --  Return the name of a Penpal, or raise Sender_Error if the name
   --  has not been set.

   procedure New_Message
     (Sender    : String;
      Recipient : access Penpal_Type;
      Message   : String);
   --  This procedure will be called when the penpal has registered itself
   --  and a new message arrives on the BBS. Sender_Error or Message_Error
   --  will be raised if Sender or Message are empty.

private

   type String_Access is access String;

   type Penpal_Type is tagged limited record
      Name : String_Access;
   end record;

   --  Legality stuff that allows a penpal name to be transferred over the
   --  network if needed.

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Penpal : out String_Access);
   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Penpal : String_Access);
   for String_Access'Read use Read;
   for String_Access'Write use Write;

end Common;
