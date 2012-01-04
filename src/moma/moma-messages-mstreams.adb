------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               M O M A . M E S S A G E S . M S T R E A M S                --
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

package body MOMA.Messages.MStreams is

   Not_Implemented : exception;

   ------------------
   -- Read_Boolean --
   ------------------

   function Read_Boolean return Boolean is
   begin
      raise Not_Implemented;
      pragma Warnings (Off);
      return Read_Boolean;
      pragma Warnings (On);
   end Read_Boolean;

   ---------------
   -- Read_Char --
   ---------------

   function Read_Char return Character is
   begin
      raise Not_Implemented;
      pragma Warnings (Off);
      return Read_Char;
      pragma Warnings (On);
   end Read_Char;

   ----------------
   -- Read_Float --
   ----------------

   function Read_Float return Float is
   begin
      raise Not_Implemented;
      pragma Warnings (Off);
      return Read_Float;
      pragma Warnings (On);
   end Read_Float;

   ------------------
   -- Read_Integer --
   ------------------

   function Read_Integer return Integer is
   begin
      raise Not_Implemented;
      pragma Warnings (Off);
      return Read_Integer;
      pragma Warnings (On);
   end Read_Integer;

   -----------------
   -- Read_String --
   -----------------

   function Read_String return String is
   begin
      raise Not_Implemented;
      pragma Warnings (Off);
      return Read_String;
      pragma Warnings (On);
   end Read_String;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      null;
      --  XXX Not Implemented
   end Reset;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean (Value : Boolean) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
      --  XXX Not Implemented
   end Set_Boolean;

   --------------
   -- Set_Char --
   --------------

   procedure Set_Char (Value : Character) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
      --  XXX Not Implemented
   end Set_Char;

   ---------------
   -- Set_Float --
   ---------------

   procedure Set_Float (Value : Float) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
      --  XXX Not Implemented
   end Set_Float;

   -----------------
   -- Set_Integer --
   -----------------

   procedure Set_Integer (Value : Integer) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
      --  XXX Not Implemented
   end Set_Integer;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (Value : String) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
      --  XXX Not Implemented
   end Set_String;

end MOMA.Messages.MStreams;
