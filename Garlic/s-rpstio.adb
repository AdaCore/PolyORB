------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . R P C . S T R E A M _ I O                  --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Streams;           use Ada.Streams;
with System.Garlic;         use System.Garlic;
with System.Garlic.Debug;   use System.Garlic.Debug;
pragma Elaborate_All (System.Garlic.Debug);
with System.Garlic.Heart;   use System.Garlic.Heart;
with System.Garlic.Streams;
with System.Garlic.Types;
with System.Garlic.Utils;   use System.Garlic.Utils;

package body System.RPC.Stream_IO is

   --  This package needs comments ???

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_RPSTIO", "(s-rpstio): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   type Partition_Stream_Record is
      record
         Mode      : Stream_Mode;
         Incoming  : aliased Streams.Params_Stream_Type (0);
         Outgoing  : aliased Streams.Params_Stream_Type (0);
         Consumer  : Barrier_Type;
         Available : Mutex_Type;
         Critical  : Mutex_Type;
      end record;
   type Partition_Stream_Access is access Partition_Stream_Record;

   Msgcode : constant Opcode := User_Message;

   Streams : array (Partition_ID'Range) of Partition_Stream_Access;

   Any : Partition_Stream_Access renames Streams (Any_Partition);

   First_Partition : Partition_ID := Partition_ID'Last;
   Last_Partition  : Partition_ID := Partition_ID'First;

   function Fetch
     (Partition : in Partition_ID)
     return Partition_Stream_Access;

   procedure Receive
     (Partition : in Types.Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Garlic.Streams.Params_Stream_Type);

   -----------
   -- Close --
   -----------

   procedure Close
     (Stream : in out Partition_Stream_Type) is
      Str : Partition_Stream_Access := Fetch (Stream.PID);
   begin
      pragma Debug (D (D_Debug, "Close stream" & Stream.PID'Img));

      if Str.Mode = Out_Mode then
         Send (Types.Partition_ID (Stream.PID), Msgcode, Str.Outgoing'Access);
      end if;

      pragma Debug (D (D_Debug, "Close - Unlock stream" & Stream.PID'Img));
      Str.Available.Leave;
   end Close;

   -----------
   -- Fetch --
   -----------

   function Fetch
     (Partition : in Partition_ID)
     return Partition_Stream_Access is
   begin
      if Streams (Partition) = null then
         pragma Debug (D (D_Debug, "Allocate stream" & Partition'Img));
         Any.Critical.Enter;
         if Streams (Partition) = null then
            Streams (Partition) := new Partition_Stream_Record;
            if First_Partition = Partition_ID'Last
              or else
              First_Partition > Partition then
               First_Partition := Partition;
            end if;
            if Last_Partition = Partition_ID'First
              or else
              Last_Partition < Partition then
               Last_Partition := Partition;
            end if;
         end if;
         Any.Critical.Leave;
      end if;
      return Streams (Partition);
   end Fetch;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Streams (Any_Partition) := new Partition_Stream_Record;
      Receive (Msgcode, Receive'Access);
   end Initialize;

   ----------
   -- Open --
   ----------

   procedure Open
     (Stream    : in out Partition_Stream_Type;
      Partition : in     Partition_ID;
      Mode      : in     Stream_Mode) is
      Str : Partition_Stream_Access;
   begin
      pragma Debug (D (D_Debug, "Open stream" & Partition'Img));

      if Mode = Out_Mode
        and then Partition = Any_Partition then
         pragma Debug (D (D_Exception, "Can't write to all partitions"));
         raise Stream_Error;
      end if;

      Str := Fetch (Partition);
      Stream.PID := Partition;

      pragma Debug (D (D_Debug, "Open - Lock stream" & Partition'Img));
      Str.Available.Enter;
      Str.Mode := Mode;

      pragma Debug (D (D_Debug, "Open - Resume stream" & Partition'Img));
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Partition_Stream_Type;
      Item   : out    Ada.Streams.Stream_Element_Array;
      Last   : out    Ada.Streams.Stream_Element_Offset) is
      FID : Partition_ID;
      LID : Partition_ID;
      Len : Stream_Element_Offset := 0;
      Str : Partition_Stream_Access := Fetch (Stream.PID);
   begin

      if Str.Mode /= In_Mode then
         pragma Debug (D (D_Exception, "Mode should be In_Mode"));
         raise Stream_Error;
      end if;

      pragma Debug (D (D_Debug, "Read new message"));

      while Len = 0 loop
         pragma Debug (D (D_Debug, "Read - Wait for stream" & Stream.PID'Img));
         Str.Consumer.Wait;

         if Stream.PID = Any_Partition then
            FID := First_Partition;
            LID := Last_Partition;
         else
            FID := Stream.PID;
            LID := Stream.PID;
         end if;

         for P in FID .. LID loop
            pragma Debug (D (D_Debug, "Read - Lock stream" & P'Img));
            Streams (P).Critical.Enter;

            pragma Debug (D (D_Debug, "Read from stream" & P'Img));
            System.Garlic.Streams.Read (Streams (P).Incoming, Item, Len);

            pragma Debug (D (D_Debug, "Read - Unlock stream" & P'Img));
            Streams (P).Critical.Leave;

            if Len /= 0 then
               if Streams (P).Incoming.Count /= 0 then
                  pragma Debug (D (D_Debug, "Read - Signal stream" & P'Img));
                  Streams (P).Consumer.Signal;
                  Any.Consumer.Signal;
               end if;
               exit;
            end if;
         end loop;

         exit when Len /= 0;
      end loop;
      Last := Len;
   end Read;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Partition : in Types.Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Garlic.Streams.Params_Stream_Type) is
      SEA : Stream_Element_Array (1 .. Params.Count);
      Len : Stream_Element_Offset;
      Str : Partition_Stream_Access := Fetch (Partition_ID (Partition));
   begin
      pragma Debug (D (D_Debug, "Receive new message"));
      pragma Debug (D (D_Debug, "Receive - Lock stream" & Partition'Img));
      Str.Critical.Enter;

      Garlic.Streams.Read (Params.all, SEA, Len);
      Garlic.Streams.Write (Str.Incoming, SEA);

      pragma Debug (D (D_Debug, "Receive - Unlock stream" & Partition'Img));
      Str.Critical.Leave;

      pragma Debug (D (D_Debug, "Signal to all streams"));
      Str.Consumer.Signal;
      Any.Consumer.Signal;
   end Receive;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Partition_Stream_Type;
      Item   : in     Ada.Streams.Stream_Element_Array) is
      Str : Partition_Stream_Access := Fetch (Stream.PID);
   begin
      pragma Debug (D (D_Debug, "Send new message"));

      if Str.Mode /= Out_Mode then
         pragma Debug (D (D_Exception, "Mode should be Out_Mode"));
         raise Stream_Error;
      end if;

      pragma Debug (D (D_Debug, "Write - Lock stream" & Stream.PID'Img));
      Str.Critical.Enter;

      pragma Debug (D (D_Debug, "Write to stream" & Stream.PID'Img));
      Garlic.Streams.Write (Str.Outgoing, Item);

      pragma Debug (D (D_Debug, "Write - Unlock stream" & Stream.PID'Img));
      Str.Critical.Leave;
   end Write;

end System.RPC.Stream_IO;
