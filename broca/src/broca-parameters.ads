package Broca.Parameters is

   pragma Pure;

   Server_Tasks_Storage_Size : constant := 1_000_000;
   --  This parameter sets the storage size for server tasks. It should
   --  depend on your particular needs: if an application passes a lot of
   --  data around and crashes, it may be useful to increase this value.

end Broca.Parameters;
