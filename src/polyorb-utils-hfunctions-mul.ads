------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . U T I L S . H F U N C T I O N S . M U L          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a simple multiplicative hasing functions for strings.

--  Hash_Mul computes for string S = (Si)i the sum of the elements
--     Hi (S) = (( K * Si ) mod Prime) mod Size

--  $Id$

package PolyORB.Utils.HFunctions.Mul is

   pragma Preelaborate;

   function Hash_Mul
     (S     : String;
      K     : Natural;
      Prime : Natural;
      Size  : Natural)
     return Natural;

   type Hash_Mul_Parameters is private;

   Default_Hash_Mul_Parameters : constant Hash_Mul_Parameters;

   function Hash_Mul
     (S     : String;
      Param : Hash_Mul_Parameters;
      Size  : Natural)
     return Natural;

   function Next_Hash_Mul_Parameters
     (Param : Hash_Mul_Parameters)
     return Hash_Mul_Parameters;

private
   type Hash_Mul_Parameters is record
      K     : Natural;
      Prime : Natural;
   end record;

   Default_Prime : constant := 1777771;

   Default_Hash_Mul_Parameters : constant Hash_Mul_Parameters
     := Hash_Mul_Parameters'(K => 1, Prime => Default_Prime);

end PolyORB.Utils.HFunctions.Mul;
