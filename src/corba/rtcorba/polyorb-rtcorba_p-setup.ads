------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . R T C O R B A _ P . S E T U P               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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

--  Implementation Notes: RTCORBA specifications defines objects that
--  are (Ada) programming language objects rather than CORBA
--  objects. Therefore the normal mechanism for coupling an
--  implementation to the code that uses it (an object reference) does
--  not apply. The implementation must provide specific mechanisms to
--  enable this coupling.
--
--  This package provides accessors to configure them. It supports the
--  following objects:
--  * PriorityMapping
--  * PriorityTransform

with RTCORBA.PriorityMapping;
with RTCORBA.PriorityTransform;

package PolyORB.RTCORBA_P.Setup is

   --  PriorityMapping

   type PriorityMapping_Access is
     access all RTCORBA.PriorityMapping.Object'Class;

   procedure Set_Priority_Mapping
     (Mapping : RTCORBA.PriorityMapping.Object'Class);
   pragma Inline (Set_Priority_Mapping);
   --  Set RT-ORB PriorityMapping object,
   --  overrides previous settings, if any.

   function Get_Priority_Mapping return PriorityMapping_Access;
   pragma Inline (Get_Priority_Mapping);
   --  Return RT-ORB PriorityMapping object.

   --  PriorityTransform

   type PriorityTransform_Access is
     access all RTCORBA.PriorityTransform.Object'Class;

   procedure Set_Priority_Transform
     (Transform : RTCORBA.PriorityTransform.Object'Class);
   pragma Inline (Set_Priority_Transform);
   --  Set RT-ORB global Priority Mapping object,
   --  overrides previous settings, if any.

   function Get_Priority_Transform return PriorityTransform_Access;
   pragma Inline (Get_Priority_Transform);
   --  Return RT-ORB global Priority Mapping object.

end PolyORB.RTCORBA_P.Setup;
