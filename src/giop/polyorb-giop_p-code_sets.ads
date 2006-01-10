------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . G I O P _ P . C O D E _ S E T S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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

   --  Default Code_Set_Ids, as defined by the Open Software Foundation.

   Latin_1_Code_Set : constant Code_Set_Id := 16#00010001#;
   UCS_2_Code_Set   : constant Code_Set_Id := 16#00010100#;
   UTF_16_Code_Set  : constant Code_Set_Id := 16#00010109#;
   UTF_8_Code_Set   : constant Code_Set_Id := 16#05010001#;

   --  Fallback Code_Set_Ids, defined by the CORBA specifications.

   Char_Data_Fallback_Code_Set  : constant Code_Set_Id := UTF_8_Code_Set;
   Wchar_Data_Fallback_Code_Set : constant Code_Set_Id := UTF_16_Code_Set;

   --  Ada95 Native Code_Set_Ids. See RM 3.5.2

   Ada95_Native_Character_Code_Set : constant Code_Set_Id := Latin_1_Code_Set;
   Ada95_Native_Wide_Character_Code_Set : constant Code_Set_Id
     := UCS_2_Code_Set;

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
     TCS      :    out Code_Set_Id;
     Error    : in out PolyORB.Errors.Error_Container);
   --  Proceed code set negotiation based on:
   --   - CNCS - Client Native Code Set
   --   - CCCS - Client Conversion Code Sets
   --   - SNCS - Server Native Code Set
   --   - SCCS - Server Conversion Code Sets
   --  Fallback argument provide fallback code sets.
   --  Return negotiated transmission code set (TCS) or throw
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
