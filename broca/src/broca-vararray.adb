------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       B R O C A . V A R A R R A Y                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
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

with Ada.Unchecked_Deallocation;

package body Broca.Vararray is
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Object => Element_Array, Name => Var_Array_Type);

   procedure Insert (Varray : in out Var_Array_Type; El : Element) is
      N_Var : Var_Array_Type;
   begin
      if Varray = null then
         Varray := new Element_Array'
           (First .. First + Initial_Size - 1 => Null_Element);
         Varray (First) := El;
         return;
      else
         --  Find a free slot.
         for I in Varray.all'Range loop
            if Varray (I) = Null_Element then
               Varray (I) := El;
               return;
            end if;
         end loop;
         N_Var := new Element_Array
           (First .. First - 1 + Varray.all'Length * 2);
         N_Var (Varray.all'Range) := Varray.all;
         N_Var (Varray.all'Last + 1) := El;
         Unchecked_Deallocation (Varray);
         Varray := N_Var;
         return;
      end if;
   end Insert;

   procedure Remove (Varray : in out Var_Array_Type; El : Element) is
   begin
      if El = Null_Element then
         raise Constraint_Error;
      end if;
      for I in Varray.all'Range loop
         if Varray (I) = El then
            Varray (I) := Null_Element;
            return;
         end if;
      end loop;
      raise Constraint_Error;
   end Remove;

   procedure Iterate (Varray : in out Var_Array_Type;
                      Iterator : Iterator_Type;
                      Arg : Boolean)
   is
   begin
      if Varray = null then
         return;
      end if;
      for I in Varray.all'Range loop
         if Varray (I) /= Null_Element then
            Iterator (Varray (I), Arg);
         end if;
      end loop;
   end Iterate;

   function Get_Element (Varray : Var_Array_Type; Index : Index_Type)
     return Element is
   begin
      return Varray (Index);
   end Get_Element;

   function Bad_Index (Varray : Var_Array_Type; Index : Index_Type)
     return Boolean is
   begin
      if Varray = null or else Index not in Varray.all'Range then
         return False;
      else
         return True;
      end if;
   end Bad_Index;

end Broca.Vararray;
