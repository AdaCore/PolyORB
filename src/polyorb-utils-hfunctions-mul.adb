------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . U T I L S . H F U N C T I O N S . M U L          --
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

package body PolyORB.Utils.HFunctions.Mul is

   Default_Prime : constant := 1_777_771;

   --------------
   -- Hash_Mul --
   --------------

   function Hash_Mul
     (S     : String;
      K     : Natural;
      Prime : Natural;
      Size  : Natural)
      return Natural
   is
      Lambda : constant := 65599;

      Result : Long_Long_Integer := 0;
   begin
      for J in S'Range loop
         Result := (Result * Lambda
                    + Long_Long_Integer (Character'Pos (S (J)))
                    * Long_Long_Integer (K))
           mod Long_Long_Integer (Prime);
      end loop;

      return Natural (Result mod Long_Long_Integer (Size));
   end Hash_Mul;

   ----------
   -- Hash --
   ----------

   overriding function Hash
     (S     : String;
      Param : Hash_Mul_Parameters;
      Size  : Natural)
     return Natural is
   begin
      return Hash_Mul (S, Param.K, Param.Prime, Size);
   end Hash;

   -----------------------------
   -- Default_Hash_Parameters --
   -----------------------------

   overriding function Default_Hash_Parameters
     return Hash_Mul_Parameters is
   begin
      return Hash_Mul_Parameters'(K => 1, Prime => Default_Prime);
   end Default_Hash_Parameters;

   --------------------------
   -- Next_Hash_Parameters --
   --------------------------

   overriding function Next_Hash_Parameters
     (Param : Hash_Mul_Parameters)
     return Hash_Mul_Parameters is
   begin
      return Hash_Mul_Parameters'
        (K => 1 + (Param.K mod Param.Prime), Prime => Param.Prime);
   end Next_Hash_Parameters;

end PolyORB.Utils.HFunctions.Mul;
