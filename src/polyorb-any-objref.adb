------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . A N Y . O B J R E F                    --
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

--  Any's that contain object references.

package body PolyORB.Any.ObjRef is

   --  'Object Reference' content

   package Elementary_Any_Ref is
     new Elementary_Any (References.Ref, Tk_Objref);

   procedure Set_Any_Value
     (X : References.Ref; C : in out Any_Container'Class)
      renames Elementary_Any_Ref.Set_Any_Value;

   function To_Any_Instance is
     new To_Any_G (References.Ref, TC_Object, Set_Any_Value);
   function To_Any (X : References.Ref) return Any renames To_Any_Instance;

   function From_Any (A : Any) return References.Ref
                      renames Elementary_Any_Ref.From_Any;
   function From_Any (C : Any_Container'Class) return References.Ref
                      renames Elementary_Any_Ref.From_Any;

   ---------
   -- Wrap --
   ----------

   function Wrap (X : access References.Ref) return Content'Class
                  renames Elementary_Any_Ref.Wrap;

end PolyORB.Any.ObjRef;
