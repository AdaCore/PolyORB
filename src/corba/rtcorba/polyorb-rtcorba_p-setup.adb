------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . R T C O R B A _ P . S E T U P               --
--                                                                          --
--                                 B o d y                                  --
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

--  $Id$

package body PolyORB.RTCORBA_P.Setup is

   Current_Priority_Mapping : PriorityMapping_Access;
   Current_Priority_Transform : PriorityTransform_Access;

   --------------------------
   -- Set_Priority_Mapping --
   --------------------------

   procedure Set_Priority_Mapping
     (Mapping : RTCORBA.PriorityMapping.Object'Class)
   is
   begin
      if Current_Priority_Mapping = null then
         Current_Priority_Mapping
           := new RTCORBA.PriorityMapping.Object'Class'(Mapping);
      else
         Current_Priority_Mapping.all := Mapping;
      end if;
   end Set_Priority_Mapping;

   --------------------------
   -- Get_Priority_Mapping --
   --------------------------

   function Get_Priority_Mapping return PriorityMapping_Access is
   begin
      return Current_Priority_Mapping;
   end Get_Priority_Mapping;

   ----------------------------
   -- Set_Priority_Transform --
   ----------------------------

   procedure Set_Priority_Transform
     (Transform : RTCORBA.PriorityTransform.Object'Class)
   is
   begin
      if Current_Priority_Transform = null then
         Current_Priority_Transform
           := new RTCORBA.PriorityTransform.Object'Class'(Transform);
      else
         Current_Priority_Transform.all := Transform;
      end if;
   end Set_Priority_Transform;

   ----------------------------
   -- Get_Priority_Transform --
   ----------------------------

   function Get_Priority_Transform return PriorityTransform_Access is
   begin
      return Current_Priority_Transform;
   end Get_Priority_Transform;

end PolyORB.RTCORBA_P.Setup;
