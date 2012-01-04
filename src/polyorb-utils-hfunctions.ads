------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . U T I L S . H F U N C T I O N S              --
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

--  Root package of Hash functions.

--  Some definitions used in child packages:

--  Universality:

--  Note : this definition is extracted from "Universal Classes of
--  Hash Functions" by J.L Carter and M. N. Wegman, Proceedings of the
--  ninth annual ACM symposium on Theory of computing, 1977.

--  A class H of hashing functions {(h_i) : U -> R, i in I} is
--  universal if for any (x,y) from U, x /= y, and h randomly chosen in H,
--  P[ h (x) = h (y) ] < 1/|R|

package PolyORB.Utils.HFunctions is

   pragma Pure;

   --  Each child package must implement the following functions to be
   --  used as hash functions class by HTables packages:

   type Hash_Parameters is abstract tagged private;
   --  Hash_Parameters holds information that uniquely identify one
   --  member of a hash functions class. It is a placeholder for this
   --  specific hash function parameters.

   function Hash
     (S     : String;
      Param : Hash_Parameters;
      Size  : Natural)
     return Natural is abstract;
   --  Hash the key S.

   function Default_Hash_Parameters
     return Hash_Parameters is abstract;
   --  Return default Hash_Parameters.

   function Next_Hash_Parameters
     (Param : Hash_Parameters)
     return Hash_Parameters is abstract;
   --  Return next Hash_Parameters.

private

   type Hash_Parameters is abstract tagged null record;

end PolyORB.Utils.HFunctions;
