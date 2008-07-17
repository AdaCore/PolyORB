------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 S Y S T E M . S H A R E D _ S T O R A G E                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2008, Free Software Foundation, Inc.          --
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

with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;

with System.DSA_Types;

with PolyORB.Any;
with PolyORB.DSA_P.Storages;
with PolyORB.DSA_P.Streams;

package body System.Shared_Storage is

   use PolyORB.Any;
   use PolyORB.DSA_P.Storages;
   use PolyORB.DSA_P.Streams;

   package SDT renames System.DSA_Types;

   --------------------------
   -- Unchecked_Conversion --
   --------------------------

   function DAC_To_AC is
     new Ada.Unchecked_Conversion
       (SDT.Any_Container_Ptr, Any_Container_Ptr);
   --  Convert Any_Container_Ptr DSA type to PolyORB one

   function AC_To_DAC is
     new Ada.Unchecked_Conversion
       (Any_Container_Ptr, SDT.Any_Container_Ptr);
   --  Convert Any_Container_Ptr PolyORB type to DSA one

   ---------------------
   -- Shared_Var_Lock --
   ---------------------

   procedure Shared_Var_Lock (Var : String) is
      SDM  : Shared_Data_Manager_RACW;

   begin
      Lookup_Variable (Var, SDM);
      Lock (SDM);
   end Shared_Var_Lock;

   -----------------------
   -- Shared_Var_Unlock --
   -----------------------

   procedure Shared_Var_Unlock (Var : String) is
      SDM  : Shared_Data_Manager_RACW;

   begin
      Lookup_Variable (Var, SDM);
      Unlock (SDM);
   end Shared_Var_Unlock;

   ----------------------
   -- Shared_Var_Procs --
   ----------------------

   package body Shared_Var_Procs is

      package SIO renames Ada.Streams.Stream_IO;
      --  XXX for instance, we use stream attributes to
      --  assign variable V of limited type Typ.

      S : constant SIO.Stream_Access :=
            new Memory_Resident_Stream (16384);
      --  XXX stream used to copy value of variable V
      --  of limited type Typ in Read routine.

      ----------
      -- Read --
      ----------

      procedure Read is
         pragma Warnings (Off);
         SDM      : Shared_Data_Manager_RACW;
         Var_Any  : Any;
         Any_Cont : SDT.Any_Container_Ptr;

      begin
         Lookup_Variable (Full_Name, SDM);
         Read (SDM, Any_Cont);
         Set_Container (Var_Any, DAC_To_AC (Any_Cont));
         if not Is_Empty (Var_Any) then

            --  V := Typ'From_Any (Var_Any)

            Typ'Write (S, Typ'From_Any (Var_Any));
            Typ'Read  (S, V);
         end if;
      end Read;

      -----------
      -- Write --
      -----------

      procedure Write is
         SDM      : Shared_Data_Manager_RACW;
         Any_Cont : SDT.Any_Container_Ptr;
         Var_Any  : Any;

      begin
         Lookup_Variable (Full_Name, SDM);
         Var_Any  := Typ'To_Any (V);
         Any_Cont := AC_To_DAC (Get_Container (Var_Any));
         Write (SDM, Any_Cont);
      end Write;

   end Shared_Var_Procs;

end System.Shared_Storage;
