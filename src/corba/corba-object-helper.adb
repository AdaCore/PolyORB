------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . O B J E C T . H E L P E R                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Any.ObjRef;
with PolyORB.CORBA_P.Local;

package body CORBA.Object.Helper is

   use PolyORB.Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any) return CORBA.Object.Ref is
      Result : CORBA.Object.Ref;
   begin
      CORBA.Object.Internals.Convert_To_CORBA_Ref
        (PolyORB.Any.ObjRef.From_Any (Item.The_Any),
         Result);

      return Result;
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : CORBA.Object.Ref) return Any is
   begin
      --  To_Any operation are not defined on local objects

      if not Is_Nil (Item)
        and then PolyORB.CORBA_P.Local.Is_Local (Item)
      then
         Raise_Marshal (Marshal_Members'(Minor     => 4,
                                         Completed => Completed_No));
      end if;

      declare
         A : Any;

      begin
         A.The_Any := PolyORB.Any.ObjRef.To_Any
           (CORBA.Object.Internals.To_PolyORB_Ref (Item));
         CORBA.Internals.Set_Type (A, CORBA.Object.TC_Object);

         return A;
      end;
   end To_Any;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (X : access CORBA.Object.Ref) return PolyORB.Any.Content'Class is
   begin
      return PolyORB.Any.ObjRef.Wrap
        (PolyORB.References.Ref (X.all)'Unrestricted_Access);
   end Wrap;

end CORBA.Object.Helper;
