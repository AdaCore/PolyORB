------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . B I N D I N G _ O B J E C T S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Binding objects: protocol stacks seen globally as a reference-counted
--  entity.

--  $Id$

with PolyORB.Filters.Interface;

package body PolyORB.Binding_Objects is

   use type PolyORB.Components.Component_Access;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (X : in out Binding_Object)
   is
      M : Filters.Interface.Disconnect_Request;
   begin
      Components.Emit_No_Reply (X.BO_Component, M);
      X.BO_Component := null;
   end Finalize;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component
     (X : Binding_Object)
      return PolyORB.Components.Component_Access
   is
   begin
      return X.BO_Component;
   end Get_Component;

   -------------------
   -- Set_Component --
   -------------------

   procedure Set_Component
     (X : in out Binding_Object;
      C :        Components.Component_Access)
   is
   begin
      pragma Assert (X.BO_Component = null xor C = null);
      X.BO_Component := C;
   end Set_Component;

end PolyORB.Binding_Objects;
