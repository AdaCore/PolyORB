------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          X E _ S T O R A G E S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2008, Free Software Foundation, Inc.               --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.HTable;

with XE_Types; use XE_Types;

--  This package contains properties of differents storages support
--  for shared memory.

package XE_Storages is

   --  Define contraints of shared storage supports
   --
   --  * Allow_Passive    : Indicate if the storage support can be
   --                       used on passive partitions
   --  * Allow_Local_Term : Indicate if the storage support can be
   --                       used on passive partitions
   --  * Need_tasking     : Indicate if the storage support need
   --                       a full tasking profile

   type Storage_Support_Type is record
      Allow_Passive    : Boolean;
      Allow_Local_Term : Boolean;
      Need_Tasking     : Boolean;
   end record;

   Unknown_Storage_Support : Storage_Support_Type;
   --  Variable returned when no storage found in table

   subtype Hash_Header is Natural range 0 .. 10;

   function Hash  (N : Name_Id)      return Hash_Header;
   function Equal (N1, N2 : Name_Id) return Boolean;
   --  Hash and equality functions for hash table

   package Storage_Supports is new GNAT.HTable.Simple_HTable
     (Header_Num => Hash_Header,
      Element    => Storage_Support_Type,
      No_Element => Unknown_Storage_Support,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => Equal);
   --  Storage support implementation repostory table

   procedure Register_Storage
     (Storage_Name     : String;
      Allow_Passive    : Boolean;
      Allow_Local_Term : Boolean;
      Need_Tasking     : Boolean);
   --  Register an available storgae support. Storage name must
   --  be a valid subpackge name of the storage package.
   --  (ex. "dsm" => PolyORB.DSA_P.Storages.DSM)

end XE_Storages;
