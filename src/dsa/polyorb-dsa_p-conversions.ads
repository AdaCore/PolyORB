------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . D S A _ P . C O N V E R S I O N S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2008-2012, Free Software Foundation, Inc.          --
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

--  This unit gathers unchecked conversions used in dsa personality.

with Ada.Unchecked_Conversion;

with PolyORB.Any;

--  WAG:601
--  pragma Warnings (Off) with pattern not supported in that compiler version
--  so use plain pragma Warnings (Off/On) instead.
--  pragma Warnings (Off, "* is an internal GNAT unit");
--  pragma Warnings (Off, "use of this unit is non-portable*");

pragma Warnings (Off);
with System.DSA_Types;
pragma Warnings (On);

package PolyORB.DSA_P.Conversions is

   pragma Warnings (Off);
   --  No strict aliasing issues, since System.DSA_Types.Any_Container_Ptr
   --  is a dummy type and it only ever used in the context of unchecked
   --  conversions from and to PolyORB.Any.Any_Container_Ptr.

   function DAC_To_AC is
     new Ada.Unchecked_Conversion
       (System.DSA_Types.Any_Container_Ptr, PolyORB.Any.Any_Container_Ptr);
   --  Convert Any_Container_Ptr DSA type to PolyORB one

   function AC_To_DAC is
     new Ada.Unchecked_Conversion
       (PolyORB.Any.Any_Container_Ptr, System.DSA_Types.Any_Container_Ptr);
   --  Convert Any_Container_Ptr PolyORB type to DSA one

   pragma Warnings (On);

end PolyORB.DSA_P.Conversions;
