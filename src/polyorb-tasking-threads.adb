------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . T A S K I N G . T H R E A D S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

package body PolyORB.Tasking.Threads is

   My_Thread_Factory : Thread_Factory_Access;
   --  Thread_Factory of the chosen profile.

   Initialised       : Boolean := False;

   -----------------
   -- Awake_Count --
   -----------------

   function Awake_Count return Natural is
   begin
      return Awake_Count (My_Thread_Factory);
   end Awake_Count;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task (Main : Parameterless_Procedure) is
      T : constant Thread_Access :=
            Run_In_Task
              (TF => My_Thread_Factory,
               P  => Main);
      pragma Warnings (Off);
      pragma Unreferenced (T);
      pragma Warnings (On);
   begin
      null;
   end Create_Task;

   ------------------
   -- Current_Task --
   ------------------

   function Current_Task
     return Thread_Id is
   begin
      return Get_Current_Thread_Id (My_Thread_Factory);
   end Current_Task;

   ------------------------
   -- Get_Thread_Factory --
   ------------------------

   function Get_Thread_Factory
     return Thread_Factory_Access is
   begin
      pragma Assert (Initialised);

      return My_Thread_Factory;
   end Get_Thread_Factory;

   -----------
   -- Image --
   -----------

   function Image (TID : Thread_Id)
                  return String is
   begin
      return Thread_Id_Image (My_Thread_Factory, TID);
   end Image;

   -----------------------
   -- Independent_Count --
   -----------------------

   function Independent_Count return Natural is
   begin
      return Independent_Count (My_Thread_Factory);
   end Independent_Count;

   -----------------------------
   -- Register_Thread_Factory --
   -----------------------------

   procedure Register_Thread_Factory
     (TF : Thread_Factory_Access) is
   begin
      pragma Assert (not Initialised);

      if not Initialised then
         My_Thread_Factory := TF;
         Initialised := True;
      end if;
   end Register_Thread_Factory;

   --------------------
   -- Relative_Delay --
   --------------------

   procedure Relative_Delay (D : Duration) is
   begin
      Relative_Delay (My_Thread_Factory, D);
   end Relative_Delay;

   --------------------
   -- Null_Thread_Id --
   --------------------

   function Null_Thread_Id return Thread_Id is
   begin
      return Thread_Id (System.Null_Address);
   end Null_Thread_Id;

   ----------------
   -- To_Address --
   ----------------

   function To_Address (TID : Thread_Id)
                       return System.Address is
   begin
      return System.Address (TID);
   end To_Address;

   ------------------
   -- To_Thread_Id --
   ------------------

   function To_Thread_Id (A : System.Address)
                         return Thread_Id is
   begin
      return Thread_Id (A);
   end To_Thread_Id;

end PolyORB.Tasking.Threads;
