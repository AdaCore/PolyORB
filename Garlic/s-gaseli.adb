------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--           S Y S T E M . G A R L I C . S E R I A L _ L I N E              --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
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
--               GARLIC is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;
with System.Garlic.Heart; use System.Garlic.Heart;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Termination; use System.Garlic.Termination;
with System.Garlic.Utils; use System.Garlic.Utils;

package body System.Garlic.Serial_Line is

   --  This system implements serial communication. The connection is
   --  established as follow: (C = caller, R = receiver)
   --    - If the partition_ID is not known:
   --      C->R : Null_Partition_ID
   --      R->C : <new caller Partition_ID>
   --    - After this, in any case: (C & R may be reversed)
   --      C->R : <Length (Stream_Element_Count)> <Packet>
   --  The boot partition never sends the first exchange since it is
   --  not needed.

   --  In this package, we make the assumption that we can access three
   --  external programs to control the serial line:
   --     - Open_Device : this parameterless program must open the device
   --                     and make it usable for future Put_Packet and
   --                     Get_Packet. It returns 1 in case of success,
   --                     0 otherwise.
   --     - Put_Packet  : takes two arguments, a buffer and an integer
   --                     (which represents the length of the buffer). It
   --                     returns 0 if there has been an error, 1 otherwise.
   --     - Get_Packet  : takes two arguments, a buffer and an integer
   --                     (which represents the length of the buffer). It
   --                     returns 0 if these bytes have been read, 1 otherwise.

   use System.Garlic.Protocols;
   package C renames Interfaces.C;
   use type C.int;
   use type System.RPC.Partition_ID;

   function Open_Device return C.int;
   pragma Import (C, Open_Device, "open_device");

   function Put_Packet (Buffer : access Ada.Streams.Stream_Element_Array;
                        Length : C.int)
     return C.int;
   pragma Import (C, Put_Packet, "put_packet");

   function Get_Packet (Buffer : access Ada.Streams.Stream_Element_Array;
                        Length : C.int)
     return C.int;
   pragma Import (C, Get_Packet, "get_packet");

   C_Failure : constant C.int := 0;
   C_Success : constant C.int := 1;

   protected Serial_Lock is
      entry Lock;
      procedure Unlock;
   private
      Locked : Boolean := False;
   end Serial_Lock;
   --  Lock protecting access to the serial line device.

   Opened : Boolean := False;
   --  State of the serial channel.

   Other_Partition : System.RPC.Partition_ID;
   --  Number of the other partition.

   Master : Boolean;
   --  True if I am the master.

   procedure Open_Connection;
   --  Connect to the other end.

   function Ask_For_Partition_ID return System.RPC.Partition_ID;
   --  Ask the remote side for a new Partition_ID.

   procedure Send_My_Partition_ID;
   --  Send the remote side my Partition_ID.

   function Get_Length_Size return Ada.Streams.Stream_Element_Count;
   --  Return the length of a Stream_Element_Count !

   task Serial_Waiter is
      entry Start;
   end Serial_Waiter;
   --  Task which will take care of receiving data.

   --------------------------
   -- Ask_For_Partition_ID --
   --------------------------

   function Ask_For_Partition_ID return System.RPC.Partition_ID is
      Params    : aliased System.RPC.Params_Stream_Type (0);
      Result    : aliased System.RPC.Params_Stream_Type (0);
      Partition : System.RPC.Partition_ID;
   begin
      System.RPC.Partition_ID'Write (Params'Access, Null_Partition_ID);
      declare
         Params_P : aliased Ada.Streams.Stream_Element_Array :=
           To_Stream_Element_Array (Params'Access);
      begin
         if Put_Packet (Params_P'Access, Params_P'Length) /= C_Success then
            raise System.RPC.Communication_Error;
         end if;
         if Get_Packet (Params_P'Access, Params_P'Length) /= C_Success then
            raise System.RPC.Communication_Error;
         end if;
         To_Params_Stream_Type (Params_P, Result'Access);
         System.RPC.Partition_ID'Read (Result'Access, Partition);
      end;
      if not Partition'Valid then
         raise Constraint_Error;
      end if;
      return Partition;
   end Ask_For_Partition_ID;

   ------------
   -- Create --
   ------------

   function Create return Protocol_Access is
      Self : constant Protocol_Access := new Serial_Protocol;
   begin
      Serial_Lock.Lock;
      Open_Connection;
      Serial_Waiter.Start;
      Serial_Lock.Unlock;
      Register_Protocol (Self);
      return Self;
   end Create;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (P : access Serial_Protocol) return String is
   begin
      return "";
   end Get_Info;

   ---------------------
   -- Get_Length_Size --
   ---------------------

   function Get_Length_Size return Ada.Streams.Stream_Element_Count is
      P : aliased System.RPC.Params_Stream_Type (0);
   begin
      Ada.Streams.Stream_Element_Count'Write (P'Access, 1);
      return To_Stream_Element_Array (P'Access) 'Length;
   end Get_Length_Size;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (P : access Serial_Protocol) return String is
   begin
      return "serial";
   end Get_Name;

   ---------------------
   -- Open_Connection --
   ---------------------

   procedure Open_Connection is
      Partition : constant System.RPC.Partition_ID :=
        Get_My_Partition_ID_Immediately;
   begin
      if Open_Device /= C_Success then
         raise System.RPC.Communication_Error;
      end if;
      if Partition /= Get_Boot_Server then
         if Partition = Null_Partition_ID then
            Set_My_Partition_ID (Ask_For_Partition_ID);
         else
            Send_My_Partition_ID;
         end if;
      end if;
   end Open_Connection;

   ----------
   -- Send --
   ----------

   procedure Send
     (Protocol  : access Serial_Protocol;
      Partition : in System.RPC.Partition_ID;
      Data      : access Ada.Streams.Stream_Element_Array)
   is
   begin
      Serial_Lock.Lock;
      if Put_Packet (Data, Data'Length) /= C_Success then
         Serial_Lock.Unlock;
         raise System.RPC.Communication_Error;
      end if;
      Serial_Lock.Unlock;
   end Send;

   --------------------------
   -- Send_My_Partition_ID --
   --------------------------

   procedure Send_My_Partition_ID is
      Params : aliased System.RPC.Params_Stream_Type (0);
   begin
      System.RPC.Partition_ID'Write (Params'Access, Get_My_Partition_ID);
      declare
         Params_P : aliased Ada.Streams.Stream_Element_Array :=
           To_Stream_Element_Array (Params'Access);
      begin
         if Put_Packet (Params_P'Access, Params_P'Length) /= C_Success then
            raise System.RPC.Communication_Error;
         end if;
      end;
   end Send_My_Partition_ID;

   -----------------
   -- Serial_Lock --
   -----------------

   protected body Serial_Lock is

      ----------
      -- Lock --
      ----------

      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;

      ------------
      -- Unlock --
      ------------

      procedure Unlock is
      begin
         Locked := False;
      end Unlock;

   end Serial_Lock;

   -------------------
   -- Serial_Waiter --
   -------------------

   task body Serial_Waiter is
      Length_Size : constant Ada.Streams.Stream_Element_Count :=
        Get_Length_Size;
   begin
      Add_Non_Terminating_Task;
      accept Start;

      if Get_My_Partition_ID_Immediately = Get_Boot_Server then
         declare
            Params    : aliased System.RPC.Params_Stream_Type (0);
            Dummy     : aliased System.RPC.Params_Stream_Type (0);
            Result    : aliased System.RPC.Params_Stream_Type (0);
         begin
            System.RPC.Partition_ID'Write (Dummy'Access, Null_Partition_ID);
            declare
               P : aliased Ada.Streams.Stream_Element_Array :=
                 To_Stream_Element_Array (Dummy'Access);
               R : System.RPC.Partition_ID;
            begin
               if Get_Packet (P'Access, P'Length) /= C_Success then
                  raise System.RPC.Communication_Error;
               end if;
               To_Params_Stream_Type (P, Params'Access);
               System.RPC.Partition_ID'Read (Params'Access, Other_Partition);
               if Other_Partition = Null_Partition_ID then
                  Other_Partition := Allocate_Partition_ID;
                  System.RPC.Partition_ID'Write
                    (Result'Access, Other_Partition);
                  P := To_Stream_Element_Array (Result'Access);
                  if Put_Packet (P'Access, P'Length) /= C_Success then
                     raise System.RPC.Communication_Error;
                  end if;
               end if;
            end;
         end;
      end if;

      loop
         declare
            Length_P : aliased Ada.Streams.Stream_Element_Array :=
              (1 .. Length_Size => 0);
            Length_I : aliased System.RPC.Params_Stream_Type (Length_Size);
            Length   : Ada.Streams.Stream_Element_Count;
         begin
            if Get_Packet (Length_P'Access, Length_P'Size) /= C_Success then
               raise System.RPC.Communication_Error;
            end if;
            To_Params_Stream_Type (Length_P, Length_I'Access);
            Ada.Streams.Stream_Element_Count'Read (Length_I'Access, Length);
            declare
               Buffer_P : aliased Ada.Streams.Stream_Element_Array :=
                 (1 .. Length => 0);
            begin
               if Get_Packet (Buffer_P'Access, Buffer_P'Size) /= C_Success then
                  raise System.RPC.Communication_Error;
               end if;
               Has_Arrived (Other_Partition, Buffer_P);
            end;
         end;
      end loop;
   end Serial_Waiter;

   -------------------
   -- Set_Boot_Data --
   -------------------

   procedure Set_Boot_Data
     (Protocol         : access Serial_Protocol;
      Is_Boot_Protocol : in Boolean := False;
      Boot_Data        : in String  := "";
      Is_Master        : in Boolean := False)
   is
   begin
      Master := Is_Master;
   end Set_Boot_Data;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Protocol : access Serial_Protocol) is
   begin
      abort Serial_Waiter;
   end Shutdown;

end System.Garlic.Serial_Line;
