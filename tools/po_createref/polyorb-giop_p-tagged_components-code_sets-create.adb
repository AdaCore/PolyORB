------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.GIOP_P.TAGGED_COMPONENTS.CODE_SETS.CREATE             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2007, Free Software Foundation, Inc.             --
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
