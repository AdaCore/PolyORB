------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . U T I L S . H F U N C T I O N S . H Y P E R        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

with PolyORB.Utils.Random;

package body PolyORB.Utils.HFunctions.Hyper is

   use PolyORB.Types;
   use PolyORB.Utils.Random;

   Default_Prime : constant := 17_771;

   ----------------
   -- Hash_Hyper --
   ----------------

   function Hash_Hyper
     (S     : String;
      Seed  : PolyORB.Types.Unsigned_Long;
      Prime : Natural;
      Size  : Natural)
     return Natural
   is
      Result : Long_Long_Integer := 0;
      G : aliased Generator;
   begin

      Reset (G'Access, Seed_Type (Seed));

      --  Loop

      for J in S'Range loop
         Result := (Result
                    + Long_Long_Integer (Random.Random (G'Access))
                    * Long_Long_Integer (Character'Pos (S (J))))
           mod Long_Long_Integer (Prime);
      end loop;

      --  Final

      return Natural (Result mod Long_Long_Integer (Size));
   end Hash_Hyper;

   ----------
   -- Hash --
   ----------

   overriding function Hash
     (S     : String;
      Param : Hash_Hyper_Parameters;
      Size  : Natural)
     return Natural is
   begin
      return Hash_Hyper (S, Param.Seed, Param.Prime, Size);
   end Hash;

   -----------------------------
   -- Default_Hash_Parameters --
   -----------------------------

   overriding function Default_Hash_Parameters return Hash_Hyper_Parameters is
   begin
      return Hash_Hyper_Parameters'(Seed => 42, Prime => Default_Prime);
   end Default_Hash_Parameters;

   --------------------------
   -- Next_Hash_Parameters --
   --------------------------

   overriding function Next_Hash_Parameters
     (Param : Hash_Hyper_Parameters) return Hash_Hyper_Parameters
   is
   begin
      return Hash_Hyper_Parameters'
        (Seed => Unsigned_Long (Hash (Param.Seed'Img, Param, Natural'Last)),
         Prime => Param.Prime);
   end Next_Hash_Parameters;

end PolyORB.Utils.HFunctions.Hyper;
