------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       B R O C A . V A R A R R A Y                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------
generic
   type Element is private;
   Null_Element : Element;
   type Index_Type is range <>;
   Initial_Size : Index_Type := 10;
   First : Index_Type := Index_Type'First;
package Broca.Vararray is
   type Var_Array_Type is private;
   Null_Var_Array : constant Var_Array_Type;

   type Iterator_Type is access procedure (El : Element; Arg : Boolean);

   procedure Insert (Varray : in out Var_Array_Type; El : Element);
   procedure Remove (Varray : in out Var_Array_Type; El : Element);
   procedure Iterate (Varray : in out Var_Array_Type;
                      Iterator : Iterator_Type;
                      Arg : Boolean);
   function Get_Element (Varray : Var_Array_Type; Index : Index_Type)
     return Element;
   function Bad_Index (Varray : Var_Array_Type; Index : Index_Type)
     return Boolean;
private
   type Element_Array is array (Index_Type range <>) of Element;
   type Var_Array_Type is access Element_Array;
   Null_Var_Array : constant Var_Array_Type := null;
end Broca.Vararray;
