with Ada.Streams;
with System.RPC;

package System.RPC.Stream_IO is

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
   --  Similar to Ada.Streams.Stream_IO.Read.

   procedure Write
     (Stream    : in out Partition_Stream_Type;
      Item      : in     Ada.Streams.Stream_Element_Array);
   --  Similar to Ada.Streams.Stream_IO.Write.

   procedure Initialize;
   --  Initialize this module.

private

   Any_Partition : constant System.RPC.Partition_ID
     := System.RPC.Partition_ID'First;

   type Partition_Stream_Type is
     new Ada.Streams.Root_Stream_Type with
      record
         PID  : System.RPC.Partition_ID;
      end record;

end System.RPC.Stream_IO;
