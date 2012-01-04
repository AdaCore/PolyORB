------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      C X E 4 0 0 5 _ P A R T _ A 1                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2012, Free Software Foundation, Inc.             --
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

-----------------------------------------------------------------------------

with Report;
with CXE4005_Part_A2;
with CXE4005_Part_B;
with CXE4005_Normal;
with CXE4005_Remote_Types;
package body CXE4005_Part_A1 is
  Root_Obj : aliased CXE4005_Common.Root_Tagged_Type;
  RT_Obj   : aliased CXE4005_Remote_Types.RT_Tagged_Type;
  Normal_Obj : aliased CXE4005_Normal.Cant_Use_In_Remote_Call;

  ---------  partition termination coordination ----------
  -- use a task to prevent the partition from completing its execution
  -- until the main procedure in partition B tells it to quit, and to insure
  -- that Report.Result is not called until after the partition is started.

  task Wait_For_Quit is
    entry Can_Quit;
    entry Quit;
  end Wait_For_Quit;

  task body Wait_For_Quit is
  begin
    accept Can_Quit;
    accept Quit;
    Report.Result;
  end Wait_For_Quit;

  procedure Can_Quit is
  begin
    Wait_For_Quit.Can_Quit;
  end Can_Quit;

  procedure Quit is
  begin
    Wait_For_Quit.Quit;
  end Quit;

  ----------

  function Get_RACWT (Which_Type : Type_Selection) 
       return CXE4005_Part_A1.RACWT is
  begin
    case Which_Type is
      when Common_Spec => return Root_Obj'Access;
      when RT_Spec     => return RT_Obj'Access;
      when B_Body      => return null;
      when Normal_Spec => return Normal_Obj'Access;
    end case;
  end Get_RACWT;

  procedure Takes_Class_Wide (X : CXE4005_Common.Open_Tagged_Type'Class) is
  begin
    CXE4005_Common.Open_Op(X);
  end Takes_Class_Wide;

  package Nested is
    type Body_Open_Tagged_Type is new CXE4005_Common.Open_Tagged_Type
          with null record;
  end Nested;

  function Return_Open_Tagged_Type_Class
    return CXE4005_Common.Open_Tagged_Type'Class is
    -- Return an object of a type not visible to the B partition.
    Obj : Nested.Body_Open_Tagged_Type;
  begin
    return Obj;
  end Return_Open_Tagged_Type_Class;

begin
  Set_Serial_Number (Root_Tagged_Type(Root_Obj)'Access,   101);
  Set_Serial_Number (Root_Tagged_Type(RT_Obj)'Access,     106);
  -- no 107 object
  Set_Serial_Number (Root_Tagged_Type(Normal_Obj)'Access, 108);
end CXE4005_Part_A1;
