------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . R T C O R B A _ P . S E T U P               --
--                                                                          --
--                                 B o d y                                  --
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
