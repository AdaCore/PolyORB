with Ada.Streams;          use Ada.Streams;
with System.RPC;           use System.RPC;
with System.Garlic.Heart;  use System.Garlic.Heart;
with System.Garlic.Utils;  use System.Garlic.Utils;

package body System.RPC.Stream_IO is

   type Partition_Stream_Record is
      record
         Incoming  : aliased Params_Stream_Type (0);
         Outgoing  : aliased Params_Stream_Type (0);
         Deferred  : Boolean := False;
         Semaphore : Semaphore_Access := new Semaphore_Type;
      end record;
   type Partition_Stream_Access is access Partition_Stream_Record;

   Msgcode : constant Opcode := User_Message;

   Streams : array (Partition_ID'Range) of Partition_Stream_Access;

   Streams_Semaphore : Semaphore_Access := new Semaphore_Type;

   procedure Check_Availability
     (Partition : in Partition_ID);

   procedure Public_Receiver
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access System.RPC.Params_Stream_Type);

   ------------------------
   -- Check_Availability --
   ------------------------

   procedure Check_Availability
     (Partition : in Partition_ID) is
   begin
      if Streams (Partition) = null then
         Streams_Semaphore.Lock;
         if Streams (Partition) = null then
            Streams (Partition) := new Partition_Stream_Record;
         end if;
         Streams_Semaphore.Unlock;
      end if;
   end Check_Availability;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Receive (Msgcode, Public_Receiver'Access);
   end Initialize;

   ---------------------
   -- Public_Receiver --
   ---------------------

   procedure Public_Receiver
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access System.RPC.Params_Stream_Type) is
      Stream : Stream_Element_Array (1 .. Params.Count);
      Dummy  : Stream_Element_Offset;
   begin

      Check_Availability (Partition);

      Streams (Partition).Semaphore.Lock;
      Read (Params.all, Stream, Dummy);
      Write (Streams (Partition).Incoming, Stream);
      Streams (Partition).Semaphore.Unlock (Modified);

   end Public_Receiver;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Partition_Stream_Type;
      Item   : out    Ada.Streams.Stream_Element_Array;
      Last   : out    Ada.Streams.Stream_Element_Offset) is
      P : Partition_ID := Stream.Partition;
      L : Stream_Element_Offset := 0;
   begin

      Check_Availability (P);

      while L = 0 loop
         Streams (P).Semaphore.Lock;
         Read (Streams (P).Incoming, Item, L);
         if L = 0 then
            Streams (P).Semaphore.Unlock (Wait_Until_Modified);
         else
            Streams (P).Semaphore.Unlock (Unmodified);
         end if;
      end loop;
      Last := L;

   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Partition_Stream_Type;
      Item   : in     Ada.Streams.Stream_Element_Array) is
      P : Partition_ID := Stream.Partition;
   begin

      Check_Availability (P);

      Streams (P).Semaphore.Lock;
      Write (Streams (P).Outgoing, Item);
      if not Streams (P).Deferred then
         Send (P, Msgcode, Streams (P).Outgoing'Access);
      end if;
      Streams (P).Semaphore.Unlock;

   end Write;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (Stream   : in Partition_Stream_Type;
      Deferred : in Boolean := False) is
      P : Partition_ID := Stream.Partition;
   begin

      Check_Availability (P);

      Streams (P).Semaphore.Lock;
      Streams (P).Deferred := Deferred;
      if not Deferred then
         Send (P, Msgcode, Streams (P).Outgoing'Access);
      end if;
      Streams (P).Semaphore.Unlock;

   end Set_Mode;

end System.RPC.Stream_IO;
