------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . O B J E C T . H E L P E R                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2017, Free Software Foundation, Inc.          --
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

with PolyORB.Any.ObjRef;
with PolyORB.CORBA_P.Local;

package body CORBA.Object.Helper is

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any) return CORBA.Object.Ref is
   begin
      return CORBA.Object.Internals.To_CORBA_Ref
        (PolyORB.Any.ObjRef.From_Any (PolyORB.Any.Any (Item)));
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
         A : Any := CORBA.Any (PolyORB.Any.ObjRef.To_Any
                      (CORBA.Object.Internals.To_PolyORB_Ref (Item)));
      begin
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
