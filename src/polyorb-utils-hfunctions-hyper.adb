------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . U T I L S . H F U N C T I O N S . H Y P E R        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

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

      G : Generator;
   begin

      Reset (G, Seed_Type (Seed));

      --  Loop

      for J in S'Range loop
         Result := (Result
                    + Long_Long_Integer (Random.Random (G))
                    * Long_Long_Integer (Character'Pos (S (J))))
           mod Long_Long_Integer (Prime);
      end loop;

      --  Final

      return Natural (Result mod  Long_Long_Integer (Size));
   end Hash_Hyper;

   ----------
   -- Hash --
   ----------

   function Hash
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

   function Default_Hash_Parameters
     return Hash_Hyper_Parameters
   is
      Default_Parameters : Hash_Hyper_Parameters;
   begin
      Default_Parameters.Seed := 42;
      Default_Parameters.Prime := Default_Prime;

      return Default_Parameters;
   end Default_Hash_Parameters;

   --------------------------
   -- Next_Hash_Parameters --
   --------------------------

   function Next_Hash_Parameters
     (Param : Hash_Hyper_Parameters)
     return Hash_Hyper_Parameters
   is
      Next_Parameters : Hash_Hyper_Parameters;
   begin
      Next_Parameters.Seed
        := Unsigned_Long (Hash (Param.Seed'Img, Param, Natural'Last));
      Next_Parameters.Prime := Param.Prime;

      return Next_Parameters;
   end Next_Hash_Parameters;

end PolyORB.Utils.HFunctions.Hyper;
