------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . R P C . S T R E A M _ I O                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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

with Ada.Streams;

package System.RPC.Stream_IO is

   pragma Elaborate_Body;

   --  This package provides message passing facilities by using streams.
   --  When a partition P1 wants to send data to another partition P2, it
   --  can open a stream with partition P1 and can send its data using
   --  'Write attributes. P2 can open a stream with P1 to receive this
   --  data by using 'Read attributes.

   Any_Partition : constant System.RPC.Partition_ID;

   type Stream_Mode is (In_Mode, Out_Mode);

   Stream_Error : exception;

   type Partition_Stream_Type is
     new Ada.Streams.Root_Stream_Type with private;

   procedure Open
     (Stream    : in out Partition_Stream_Type;
      Partition : in     System.RPC.Partition_ID;
      Mode      : in     Stream_Mode);

   procedure Close
     (Stream    : in out Partition_Stream_Type);

   procedure Read
     (Stream    : in out Partition_Stream_Type;
      Item      : out    Ada.Streams.Stream_Element_Array;
      Last      : out    Ada.Streams.Stream_Element_Offset);
   --  Similar to Ada.Streams.Stream_IO.Read

   procedure Write
     (Stream    : in out Partition_Stream_Type;
      Item      : in     Ada.Streams.Stream_Element_Array);
   --  Similar to Ada.Streams.Stream_IO.Write

   procedure Initialize;
   --  Initialize this module

private

   Any_Partition : constant System.RPC.Partition_ID
     := System.RPC.Partition_ID'First;

   type Partition_Stream_Type is
     new Ada.Streams.Root_Stream_Type with
      record
         PID  : System.RPC.Partition_ID;
         Open : Boolean := False;
      end record;

end System.RPC.Stream_IO;
