--
--  $Id$
--

with Ada.Unchecked_Deallocation;
with System.Garlic.Caching;

private package System.RPC.Util is

   --  This package contains utility routines for System.RPC and children.

   procedure Free is
      new Ada.Unchecked_Deallocation (Node, Node_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation
          (Params_Stream_Type, Params_Stream_Access);
   --  Deallocate a Params_Stream_Access.

   procedure Deep_Free (Stream : in out Params_Stream_Access);
   --  This procedure make sure that unconsumed data has been freed. This
   --  may occur in case of cancellation.

   package Receiver_Map is
      new System.Garlic.Caching (Index_Type => Partition_ID,
                                 Data_Type  => RPC_Receiver,
                                 Unset      => null);

end System.RPC.Util;
