------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . B I N D I N G _ O B J E C T S               --
--                                                                          --
--                                 S p e c                                  --
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

with PolyORB.Components;
with PolyORB.Filters;
with PolyORB.ORB;
with PolyORB.Smart_Pointers;
with PolyORB.Transport;

package PolyORB.Binding_Objects is

   type Binding_Object is new Smart_Pointers.Entity with private;
   type Binding_Object_Access is access all Binding_Object'Class;
   --  A protocol session and associated transport and filter stack,
   --  seen globally as a reference-counted entity.

   function Get_Component
     (X : Smart_Pointers.Ref)
     return PolyORB.Components.Component_Access;
   --  Return the top component of the Binding_Object designated
   --  by reference X.

   procedure Setup_Binding_Object
     (The_ORB :     ORB.ORB_Access;
      TE      :     Transport.Transport_Endpoint_Access;
      FFC     :     Filters.Factory_Array;
      Role    :     ORB.Endpoint_Role;
      BO_Ref  : out Smart_Pointers.Ref);
   --  Create a binding object associating TE with a protocol stack
   --  instantiated using FFC.

private

   type Binding_Object is new Smart_Pointers.Entity with record
      Transport_Endpoint : Transport.Transport_Endpoint_Access;
      Top : Filters.Filter_Access;
   end record;

   procedure Finalize (X : in out Binding_Object);

end PolyORB.Binding_Objects;
