--  The storage pool to be used for dynamic allocation in PolyORB.

--  $Id$

--  with System.Storage_Pools;
with GNAT.Debug_Pools;

package PolyORB.Storage_Pools is

   Debug_Pool : GNAT.Debug_Pools.Debug_Pool;

end PolyORB.Storage_Pools;
