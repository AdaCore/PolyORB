with Ada.Streams;
with System.RPC;

package System.RPC.Stream_IO is

   type Partition_Stream_Type
     (Partition : System.RPC.Partition_ID) is new
     Ada.Streams.Root_Stream_Type with private;

   procedure Read
     (Stream : in out Partition_Stream_Type;
      Item   : out    Ada.Streams.Stream_Element_Array;
      Last   : out    Ada.Streams.Stream_Element_Offset);
   --  Similar to Ada.Streams.Stream_IO.Read.

   procedure Write
     (Stream : in out Partition_Stream_Type;
      Item   : in     Ada.Streams.Stream_Element_Array);
   --  Similar to Ada.Streams.Stream_IO.Write.

   procedure Set_Mode
     (Stream   : in Partition_Stream_Type;
      Deferred : in Boolean := False);
   --  Defer or not write operations.

private

   type Partition_Stream_Type
     (Partition : System.RPC.Partition_ID) is new
     Ada.Streams.Root_Stream_Type with null record;

end System.RPC.Stream_IO;



