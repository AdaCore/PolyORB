------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . G I O P _ P . C O D E _ S E T S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Errors;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Types;

package PolyORB.GIOP_P.Code_Sets is

   type Code_Set_Id is new PolyORB.Types.Unsigned_Long;

   --  Code_Set_Ids, as defined by the Open Software Foundation.

   Latin_1_Code_Set       : constant Code_Set_Id := 16#00010001#;
   UCS_2_Level_1_Code_Set : constant Code_Set_Id := 16#00010100#;
   UCS_2_Level_2_Code_Set : constant Code_Set_Id := 16#00010101#;
   UCS_2_Level_3_Code_Set : constant Code_Set_Id := 16#00010102#;
   UCS_4_Level_1_Code_Set : constant Code_Set_Id := 16#00010104#;
   UCS_4_Level_2_Code_Set : constant Code_Set_Id := 16#00010105#;
   UCS_4_Level_3_Code_Set : constant Code_Set_Id := 16#00010106#;
   UTF_16_Code_Set        : constant Code_Set_Id := 16#00010109#;
   UTF_8_Code_Set         : constant Code_Set_Id := 16#05010001#;

   --  Fallback Code_Set_Ids, defined by the CORBA specifications.

   Char_Data_Fallback_Code_Set  : constant Code_Set_Id := UTF_8_Code_Set;
   Wchar_Data_Fallback_Code_Set : constant Code_Set_Id := UTF_16_Code_Set;

   --  Ada 95 Native Code_Set_Ids. See RM 3.5.2

   Ada95_Native_Character_Code_Set      : constant Code_Set_Id :=
                                            Latin_1_Code_Set;
   Ada95_Native_Wide_Character_Code_Set : constant Code_Set_Id :=
                                            UCS_2_Level_1_Code_Set;

   type Character_Set_Id is new PolyORB.Types.Unsigned_Short;

   package Code_Set_Id_Lists is new PolyORB.Utils.Chained_Lists (Code_Set_Id);

   type Code_Set_Id_List is new Code_Set_Id_Lists.List;

   function Native_Char_Code_Set return Code_Set_Id;
   --  Return program's native code set for Character type

   function Native_Wchar_Code_Set return Code_Set_Id;
   --  Return program's native code set for Wide_Character type

   function Conversion_Char_Code_Sets return Code_Set_Id_List;
   --  Return conversion code sets supported for program's native
   --  code set for Character type, except fallback code set.
   --  Returned list must not be deallocated.

   function Conversion_Wchar_Code_Sets return Code_Set_Id_List;
   --  Return conversion code sets supported for program's native
   --  code set for Wide_Character type, except fallback code set.
   --  Returned list must not be deallocated.

   procedure Negotiate_Code_Set
    (CNCS     : Code_Set_Id;
     CCCS     : Code_Set_Id_List;
     SNCS     : Code_Set_Id;
     SCCS     : Code_Set_Id_List;
     Fallback : Code_Set_Id;
     TCS      : out Code_Set_Id;
     Error    : in out PolyORB.Errors.Error_Container);
   --  Select transmission code set to use based on:
   --   - CNCS - Client Native Code Set
   --   - CCCS - Client Conversion Code Sets
   --   - SNCS - Server Native Code Set
   --   - SCCS - Server Conversion Code Sets
   --  Fallback argument provide fallback code sets.
   --  Returns negotiated transmission code set (TCS) or raises
   --  Codeset_Incompatible error if code sets are incompatible.

private

   type Code_Set_Info_Record is record
      Code_Set : Code_Set_Id;
      First    : Positive;
      Last     : Natural;
   end record;

   type Character_Set_Id_Array is
     array (Positive range <>) of Character_Set_Id;

end PolyORB.GIOP_P.Code_Sets;
