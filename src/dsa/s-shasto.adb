------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                S Y S T E M . S H A R E D _ S T O R A G E                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams.Stream_IO;

with System.DSA_Types;

with PolyORB.Any;
with PolyORB.DSA_P.Conversions;
with PolyORB.DSA_P.Storages;
with PolyORB.DSA_P.Streams;

package body System.Shared_Storage is

   use PolyORB.Any;
   use PolyORB.DSA_P.Conversions;
   use PolyORB.DSA_P.Storages;
   use PolyORB.DSA_P.Streams;

   package SDT renames System.DSA_Types;

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
         SDM      : Shared_Data_Manager_RACW;
         Data     : constant Any := Typ'To_Any (V);
         Data_Ptr : constant SDT.Any_Container_Ptr :=
                      AC_To_DAC (Get_Container (Data));

      begin
         Lookup_Variable (Full_Name, SDM);
         Read (SDM, Data_Ptr);
         if not Is_Empty (Data) then

            --  V := Typ'From_Any (A)

            Typ'Write (S, Typ'From_Any (Data));
            Typ'Read  (S, V);
         end if;
      end Read;

      -----------
      -- Write --
      -----------

      procedure Write is
         SDM      : Shared_Data_Manager_RACW;
         Data     : constant Any := Typ'To_Any (V);
         Data_Ptr : constant SDT.Any_Container_Ptr :=
           AC_To_DAC (Get_Container (Data));

      begin
         Lookup_Variable (Full_Name, SDM);
         Write (SDM, Data_Ptr);
      end Write;

   end Shared_Var_Procs;

end System.Shared_Storage;
