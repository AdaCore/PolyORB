------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . O B J E C T . H E L P E R                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  $Id$

with PolyORB.Any.ObjRef;

package body CORBA.Object.Helper is

   use PolyORB.Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in CORBA.Object.Ref) return Any is
   begin
      return PolyORB.Any.ObjRef.To_Any (To_PolyORB_Ref (Item));
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in Any) return CORBA.Object.Ref
   is
      Result : CORBA.Object.Ref;
   begin
      Convert_To_CORBA_Ref
        (PolyORB.Any.ObjRef.From_Any (Item),
         Result);
      return Result;
   end From_Any;

   ---------------------
   --  Set_Any_Value  --
   ---------------------

   procedure Set_Any_Value
     (Any_Value : in out CORBA.Any;
      Value : in CORBA.Object.Ref) is
   begin
      PolyORB.Any.ObjRef.Set_Any_Value
        (Any_Value, To_PolyORB_Ref (Value));
   end Set_Any_Value;

end CORBA.Object.Helper;
