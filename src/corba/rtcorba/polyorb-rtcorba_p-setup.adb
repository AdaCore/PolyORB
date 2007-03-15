------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . R T C O R B A _ P . S E T U P               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;
with PolyORB.Tasking.Priorities;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package body PolyORB.RTCORBA_P.Setup is

   use PolyORB.Tasking.Priorities;

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

   --------------------------
   -- To_External_Priority --
   --------------------------

   procedure To_External_Priority
     (Value    : ORB_Priority;
      Result   : out External_Priority;
      Returns  : out PolyORB.Types.Boolean);

   procedure To_External_Priority
     (Value    : ORB_Priority;
      Result   : out External_Priority;
      Returns  : out PolyORB.Types.Boolean)
   is
   begin
      RTCORBA.PriorityMapping.To_CORBA
        (Current_Priority_Mapping.all,
         RTCORBA.NativePriority (Value),
         RTCORBA.Priority (Result),
         Returns);
   end To_External_Priority;

   ---------------------
   -- To_ORB_Priority --
   ---------------------

   procedure To_ORB_Priority
     (Value    : External_Priority;
      Result   : out ORB_Priority;
      Returns  : out PolyORB.Types.Boolean);

   procedure To_ORB_Priority
     (Value    : External_Priority;
      Result   : out ORB_Priority;
      Returns  : out PolyORB.Types.Boolean)
   is
   begin
      RTCORBA.PriorityMapping.To_Native
        (Current_Priority_Mapping.all,
         RTCORBA.Priority (Value),
         RTCORBA.NativePriority (Result),
         Returns);
   end To_ORB_Priority;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.Tasking.Priorities.To_External_Priority
        := To_External_Priority'Access;

      PolyORB.Tasking.Priorities.To_ORB_Priority
        := To_ORB_Priority'Access;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"rtcorba_p.setup",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.RTCORBA_P.Setup;
