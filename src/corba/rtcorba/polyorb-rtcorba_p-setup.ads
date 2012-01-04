------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . R T C O R B A _ P . S E T U P               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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
