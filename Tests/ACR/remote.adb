with System.RPC; use System.RPC;

package body Remote is

   function Call_OK (P : Natural) return Boolean is
   begin
      return P = Remote'Partition_ID;
   end Call_OK;

end Remote;
