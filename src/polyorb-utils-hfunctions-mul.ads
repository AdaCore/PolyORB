------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . U T I L S . H F U N C T I O N S . M U L          --
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

--  This package provides a simple class of hashing functions for strings.

--  Hash_Mul computes for string S = (Si)i the sum of the elements
--     Hi (S) = (( K * Si ) mod Prime) mod Size

--  Note: this class of function is not universal.

package PolyORB.Utils.HFunctions.Mul is

   pragma Preelaborate;

   function Hash_Mul
     (S     : String;
      K     : Natural;
      Prime : Natural;
      Size  : Natural)
     return Natural;
   --  Hash function implemented by this package.
   --  S: key to hash,
   --  (K, Prime): Hash function parameters,
   --  Size: restrict results to range O .. Size - 1.

   type Hash_Mul_Parameters is new Hash_Parameters with private;

   overriding function Hash
     (S     : String;
      Param : Hash_Mul_Parameters;
      Size  : Natural)
     return Natural;

   overriding function Default_Hash_Parameters
     return Hash_Mul_Parameters;
   pragma Inline (Default_Hash_Parameters);

   overriding function Next_Hash_Parameters
     (Param : Hash_Mul_Parameters)
     return Hash_Mul_Parameters;

private

   type Hash_Mul_Parameters is new Hash_Parameters with record
      K     : Natural;
      Prime : Natural;
   end record;

end PolyORB.Utils.HFunctions.Mul;
