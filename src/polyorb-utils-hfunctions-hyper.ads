------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . U T I L S . H F U N C T I O N S . H Y P E R        --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides a class of hashing functions for strings.

--  Hash_Hyper computes for string S = (Si)i the sum of the elements
--    Hi (S) = (( Ai * Si ) mod Prime) mod Size
--  where A = (A0, A1, ..) is a random vector of dimension S'Length

--  Note: this class of function is universal.

with PolyORB.Types;

package PolyORB.Utils.HFunctions.Hyper is

   pragma Preelaborate;

   function Hash_Hyper
     (S     : String;
      Seed  : PolyORB.Types.Unsigned_Long;
      Prime : Natural;
      Size  : Natural)
     return Natural;
   --  Hash function implemented by this package.
   --  S     : key to hash,
   --  Seed  : seed for the pseudo-random generator to use,
   --  Prime : a prime number
   --  Size  : restrict results to range O .. Size - 1.
   --
   --  Note that Prime is an implicit upper bound of the length of the
   --  string to be hashed.

   type Hash_Hyper_Parameters is new Hash_Parameters with private;

   function Hash
     (S     : String;
      Param : Hash_Hyper_Parameters;
      Size  : Natural)
     return Natural;

   function Default_Hash_Parameters
     return Hash_Hyper_Parameters;

   function Next_Hash_Parameters
     (Param : Hash_Hyper_Parameters)
     return Hash_Hyper_Parameters;

private

   type Hash_Hyper_Parameters is new Hash_Parameters with record
      Seed   : PolyORB.Types.Unsigned_Long := 0;
      Prime  : Natural := 0;
   end record;

end PolyORB.Utils.HFunctions.Hyper;
