--  The storage pool to be used for dynamic allocation in DROOPI.

--  $Id$

--  with System.Storage_Pools;
with GNAT.Debug_Pools;

package Droopi.Storage_Pools is

   Debug_Pool : GNAT.Debug_Pools.Debug_Pool;

end Droopi.Storage_Pools;
