------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . R P C . S T R E A M _ I O                  --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Streams;              use Ada.Streams;
with System.Garlic;            use System.Garlic;
with System.Garlic.Debug;      use System.Garlic.Debug;
with System.Garlic.Exceptions; use System.Garlic.Exceptions;
with System.Garlic.Heart;      use System.Garlic.Heart;
with System.Garlic.Soft_Links;
with System.Garlic.Streams;
with System.Garlic.Table;
with System.Garlic.Types;

with System.Garlic.Startup;
pragma Elaborate_All (System.Garlic.Startup);
pragma Warnings (Off, System.Garlic.Startup);

package body System.RPC.Stream_IO is

   --  This package needs comments ???

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_RPSTIO", "(s-rpstio): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Msgcode : constant Any_Opcode := User_Message;

   type Partition_Stream_Record is
      record
         Mode      : Stream_Mode;
         Incoming  : aliased Streams.Params_Stream_Type (0);
         Outgoing  : aliased Streams.Params_Stream_Type (0);
         Consumer  : System.Garlic.Soft_Links.Watcher_Access;
         Available : System.Garlic.Soft_Links.Mutex_Access;
         Critical  : System.Garlic.Soft_Links.Mutex_Access;
      end record;
   type Partition_Stream_Access is access Partition_Stream_Record;

   First_Partition_Id : constant Partition_ID := Any_Partition + 1;

   package Streams is
      new System.Garlic.Table.Medium
        (Partition_ID,
         Any_Partition,
         Any_Partition,
         Types.Partition_ID_Increment,
         Types.Partition_ID_Increment,
         Partition_Stream_Access,
         null);

   Any : Partition_Stream_Access;

   function Fetch
     (Partition : in Partition_ID)
     return Partition_Stream_Access;

   procedure Handle_Request
     (Partition : in Types.Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Garlic.Streams.Params_Stream_Type;
      Reply     : access Garlic.Streams.Params_Stream_Type;
      Error     : in out Error_Type);

   -----------
   -- Close --
   -----------

   procedure Close
     (Stream : in out Partition_Stream_Type)
   is
      Err : aliased Error_Type;
      Str : Partition_Stream_Access;
   begin

      if not Stream.Open then
         raise Stream_Error;
      end if;

      Str := Streams.Get_Component (Stream.PID);

      pragma Debug (D ("Close stream" & Stream.PID'Img));

      --  When Out_Mode, procedure Close is in charge of sending the
      --  outgoing stream element array.

      if Str.Mode = Out_Mode then
         pragma Debug (D ("Send new message"));

         Send (Types.Partition_ID (Stream.PID),
               Msgcode,
               Str.Outgoing'Access,
               Err);
      end if;


      pragma Debug (D ("Close - Unlock stream" & Stream.PID'Img));
      Stream.Open := False;
      System.Garlic.Soft_Links.Leave (Str.Available);

      if Found (Err) then
         Raise_Exception
           (Communication_Error'Identity,
            Content (Err'Access));
      end if;
   exception when others =>
      pragma Debug (D ("exception raised in Close"));
      null;
   end Close;

   -----------
   -- Fetch --
   -----------

   function Fetch
     (Partition : in Partition_ID)
     return Partition_Stream_Access
   is
      Stream : Partition_Stream_Access := Streams.Get_Component (Partition);
   begin
      if Stream = null then
         Streams.Enter;
         Stream := Streams.Get_Component (Partition);
         if Stream = null then
            pragma Debug (D ("Allocate stream" & Partition'Img));
            Stream := new Partition_Stream_Record;
            System.Garlic.Soft_Links.Create (Stream.Consumer);
            System.Garlic.Soft_Links.Create (Stream.Available);
            System.Garlic.Soft_Links.Create (Stream.Critical);
            Streams.Set_Component (Partition, Stream);
         end if;
         Streams.Leave;
      end if;
      return Stream;
   end Fetch;

   --------------------
   -- Handle_Request --
   --------------------

   procedure Handle_Request
     (Partition : in Types.Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Garlic.Streams.Params_Stream_Type;
      Reply     : access Garlic.Streams.Params_Stream_Type;
      Error     : in out Error_Type)
   is
      pragma Unreferenced (Opcode);
      pragma Unreferenced (Reply);
      pragma Unreferenced (Error);

      SEA : Stream_Element_Array (1 .. Query.Count);
      Len : Stream_Element_Offset;
      Str : Partition_Stream_Access := Fetch (Partition_ID (Partition));
   begin
      pragma Debug (D ("Receive new message"));
      pragma Debug (D ("Receive - Lock stream" & Partition'Img));
      System.Garlic.Soft_Links.Enter (Str.Critical);

      Garlic.Streams.Read (Query.all, SEA, Len);
      Garlic.Streams.Write (Str.Incoming, SEA);

      pragma Debug (D ("Receive - Unlock stream" & Partition'Img));
      System.Garlic.Soft_Links.Leave (Str.Critical);

      --  Signal to consumer connected to Partition and to
      --  Any_Partition.

      pragma Debug (D ("Signal to all streams"));
      System.Garlic.Soft_Links.Update (Str.Consumer);
      System.Garlic.Soft_Links.Update (Any.Consumer);
   end Handle_Request;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Streams.Initialize;
      Any := new Partition_Stream_Record;
      System.Garlic.Soft_Links.Create (Any.Consumer);
      System.Garlic.Soft_Links.Create (Any.Available);
      System.Garlic.Soft_Links.Create (Any.Critical);
      Streams.Set_Component (Any_Partition, Any);
      Register_Handler (Msgcode, Handle_Request'Access);
   end Initialize;

   ----------
   -- Open --
   ----------

   procedure Open
     (Stream    : in out Partition_Stream_Type;
      Partition : in     Partition_ID;
      Mode      : in     Stream_Mode)
   is
      Str : Partition_Stream_Access;
   begin

      if Stream.Open then
         raise Stream_Error;
      end if;

      Stream.Open := True;

      pragma Debug (D ("Open stream" & Partition'Img));
      if Mode = Out_Mode
        and then Partition = Any_Partition
      then
         pragma Debug (D ("Can't write to all partitions"));
         raise Stream_Error;
      end if;

      Str := Fetch (Partition);
      Stream.PID := Partition;

      --  Only one task at a time

      pragma Debug (D ("Open - Lock stream" & Partition'Img));
      System.Garlic.Soft_Links.Enter (Str.Available);
      Str.Mode := Mode;

      pragma Debug (D ("Open - Resume stream" & Partition'Img));
   exception when others =>
      pragma Debug (D ("exception raised in Open"));
      null;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Partition_Stream_Type;
      Item   : out    Ada.Streams.Stream_Element_Array;
      Last   : out    Ada.Streams.Stream_Element_Offset)
   is
      FID     : Partition_ID;
      LID     : Partition_ID;
      Len     : Stream_Element_Offset := 0;
      Str     : Partition_Stream_Access;
      From    : Partition_Stream_Access;
      Version : System.Garlic.Types.Version_Id;

   begin

      if not Stream.Open then
         raise Stream_Error;
      end if;

      Str := Streams.Get_Component (Stream.PID);

      if Str.Mode /= In_Mode then
         pragma Debug (D ("Mode should be In_Mode"));
         raise Stream_Error;
      end if;

      while Len = 0 loop

         --  Is there something to read ?

         pragma Debug (D ("Read - Wait for stream" & Stream.PID'Img));

         --  For Any_Partition, look at all the partitions.

         if Stream.PID = Any_Partition then
            FID := First_Partition_Id;
            LID := Streams.Last;
         else
            FID := Stream.PID;
            LID := Stream.PID;
         end if;

         for P in FID .. LID loop
            From := Streams.Get_Component (P);
            if From /= null then

               pragma Debug (D ("Read - Lock stream" & P'Img));
               System.Garlic.Soft_Links.Enter (From.Critical);

               pragma Debug (D ("Read from stream" & P'Img));
               System.Garlic.Streams.Read (From.Incoming, Item, Len);

               pragma Debug (D ("Read - Unlock stream" & P'Img));
               System.Garlic.Soft_Links.Leave (From.Critical);

               if Len /= 0 then

                  pragma Debug (D ("Read new message"));
                  pragma Debug (D ("Read" & Len'Img & " bytes"));

                  --  There are elements left. Signal to potential
                  --  consumers.

                  if From.Incoming.Count /= 0 then
                     pragma Debug (D ("Read - Signal stream" & P'Img));
                     System.Garlic.Soft_Links.Update (From.Consumer);
                     System.Garlic.Soft_Links.Update (Any.Consumer);
                  end if;
                  exit;
               end if;
            end if;

         end loop;

         exit when Len /= 0;

         System.Garlic.Soft_Links.Lookup (Str.Consumer, Version);
         System.Garlic.Soft_Links.Differ (Str.Consumer, Version);
      end loop;
      Last := Len;
   exception when others =>
      pragma Debug (D ("exception raised in Read"));
      null;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Partition_Stream_Type;
      Item   : in     Ada.Streams.Stream_Element_Array) is
      Str : Partition_Stream_Access;
   begin
      if not Stream.Open then
         raise Stream_Error;
      end if;

      Str := Fetch (Stream.PID);

      --  Procedure Write just buffers the stream element
      --  array. Procedure Close really sends them.

      if Str.Mode /= Out_Mode then
         pragma Debug (D ("Mode should be Out_Mode"));
         raise Stream_Error;
      end if;

      pragma Debug (D ("Write - Lock stream" & Stream.PID'Img));
      System.Garlic.Soft_Links.Enter (Str.Critical);

      pragma Debug (D ("Write to stream" & Stream.PID'Img));
      Garlic.Streams.Write (Str.Outgoing, Item);

      pragma Debug (D ("Write - Unlock stream" & Stream.PID'Img));
      System.Garlic.Soft_Links.Leave (Str.Critical);
   exception when others =>
      pragma Debug (D ("exception raised in Write"));
      null;
   end Write;

begin
   Initialize;
end System.RPC.Stream_IO;
