------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                  C O R B A . O B J E C T . H E L P E R                   --
--                                                                          --
--                                 B o d y                                  --
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

with Broca.Exceptions;

package body CORBA.Object.Helper is

   --------------
   --  To_Any  --
   --------------
   function To_Any (Item : in CORBA.Object.Ref) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_ObjRef' (Value => Item));
      Set_Type (Result, TC_Object);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   ----------------
   --  From_Any  --
   ----------------
   function From_Any (Item : in Any) return CORBA.Object.Ref is
   begin
      if (TypeCode.Kind (Get_Type (Item)) /= Tk_Objref) then
         raise Bad_TypeCode;
      end if;
      return Content_ObjRef_Ptr (Get_Value (Item)).Value;
   end From_Any;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Object.Ref) is
      use CORBA.TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Precise_Type (Any_Value)) /= Tk_Objref then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value.all /= Null_Content_Ptr then
         Content_ObjRef_Ptr (Any_Value.The_Value.all).Value := Value;
      else
         Any_Value.The_Value.all := new Content_ObjRef'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

end CORBA.Object.Helper;
