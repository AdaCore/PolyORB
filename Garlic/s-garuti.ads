--
--  $Id$
--

with Ada.Streams;
with System.RPC;

private package System.Garlic.Utils is

   protected type Barrier is
      entry Wait;
      procedure Signal (How_Many : Positive := 1);
      procedure Signal_All (Permanent : Boolean);
   private
      Free : Natural := 0;
      Perm : Boolean := False;
   end Barrier;
   --  Any number of task may be waiting on Wait. Signal unblocks How_Many
   --  tasks (the order depends on the queuing policy) and Signal_All unblocks
   --  all the tasks (if Permanent is True, Wait will no longer be blocking).
   --  If How_Many is more than the number of tasks waiting, new tasks will
   --  be awakened as well.

   function To_Stream_Element_Array
     (Params : access System.RPC.Params_Stream_Type)
      return Ada.Streams.Stream_Element_Array;
   --  This routine "looks" into the Params structure to extract the
   --  Stream_Element_Array which will be sent accross the network.

   procedure To_Params_Stream_Type
     (Content : Ada.Streams.Stream_Element_Array;
      Params  : access System.RPC.Params_Stream_Type);
   --  Other way.

end System.Garlic.Utils;
