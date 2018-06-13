------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . U T I L S . H F U N C T I O N S . H Y P E R        --
--                                                                          --
--                                 S p e c                                  --
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

   overriding function Hash
     (S     : String;
      Param : Hash_Hyper_Parameters;
      Size  : Natural)
     return Natural;

   overriding function Default_Hash_Parameters
     return Hash_Hyper_Parameters;

   overriding function Next_Hash_Parameters
     (Param : Hash_Hyper_Parameters)
     return Hash_Hyper_Parameters;

private

   type Hash_Hyper_Parameters is new Hash_Parameters with record
      Seed   : PolyORB.Types.Unsigned_Long := 0;
      Prime  : Natural := 0;
   end record;

end PolyORB.Utils.HFunctions.Hyper;
