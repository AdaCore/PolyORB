------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     S Y S T E M . D S A _ T Y P E S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2008-2012, Free Software Foundation, Inc.          --
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

package body System.DSA_Types is

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Any_Container_Ptr) is
   begin
      raise Program_Error
        with "System.DSA_Types.Any_Container_Ptr'Read should not be called";
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Any_Container_Ptr) is
   begin
      raise Program_Error
        with "System.DSA_Types.Any_Container_Ptr'Write should not be called";
   end Write;

end System.DSA_Types;
