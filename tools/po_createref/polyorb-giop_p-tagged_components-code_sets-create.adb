------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.GIOP_P.TAGGED_COMPONENTS.CODE_SETS.CREATE             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with PolyORB.GIOP_P.Code_Sets;

package body PolyORB.GIOP_P.Tagged_Components.Code_Sets.Create is
   use PolyORB.GIOP_P.Tagged_Components.Code_Sets;
   use PolyORB.GIOP_P.Code_Sets;

   procedure Create_TC
     (Param      : Parameter_Component;
      TC         : in out TC_Code_Sets;
      Error      : out Boolean)
   is
   begin
      --  Code Set component BNF :
      --  -char <Char_Native_Code_Set> Supported_Code_Sets
      --  - wchar <Wchar_Native_Code_Set> Supported_Code_Sets

      --  Where "Supported_Code_Sets" :
      --  -s <Code_Sets_Number> { <Code_Set> }

      Error := False;

      --  Char_Data
      TC.For_Char_Data.Native_Code_Set :=
        Code_Set_Id'Value (Param.Cchar.all);

      for J in Param.C_Supported.all'Range loop
         Append (TC.For_Char_Data.Conversion_Code_Sets,
                 Code_Set_Id'Value (Param.C_Supported.all (J).all));
      end loop;

      --  Wchar_Data
      TC.For_Wchar_Data.Native_Code_Set :=
        Code_Set_Id'Value (Param.Wchar.all);

      for J in Param.W_Supported.all'Range loop
         Append (TC.For_Wchar_Data.Conversion_Code_Sets,
                 Code_Set_Id'Value (Param.W_Supported.all (J).all));
      end loop;

   end Create_TC;

end PolyORB.GIOP_P.Tagged_Components.Code_Sets.Create;
