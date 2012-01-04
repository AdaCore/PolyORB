------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . A N Y . O B J R E F                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Any's that contain object references.

package body PolyORB.Any.ObjRef is

   --  'Object Reference' content

   package Elementary_Any_Ref is
     new Elementary_Any (References.Ref, Tk_Objref);

   procedure Set_Any_Value
     (X : References.Ref; C : in out Any_Container'Class)
      renames Elementary_Any_Ref.Set_Any_Value;

   function To_Any_Instance is
     new To_Any_G (References.Ref, TypeCode.TC_Object, Set_Any_Value);
   function To_Any (X : References.Ref) return Any renames To_Any_Instance;

   function From_Any (A : Any) return References.Ref
                      renames Elementary_Any_Ref.From_Any;
   function From_Any (C : Any_Container'Class) return References.Ref
                      renames Elementary_Any_Ref.From_Any;

   ---------
   -- Wrap --
   ----------

   function Wrap (X : not null access References.Ref) return Content'Class
     renames Elementary_Any_Ref.Wrap;

end PolyORB.Any.ObjRef;
