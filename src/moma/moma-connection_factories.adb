------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            M O M A . C O N N E C T I O N _ F A C T O R I E S             --
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

package body MOMA.Connection_Factories is

   ------------
   -- Create --
   ------------

   procedure Create
     (Self     : out Connection_Factory;
      Remote   :     MOMA.Types.Ref) is
   begin
      Set_Ref (Self, Remote);
   end Create;

   -------------
   -- Get_Ref --
   -------------

   function Get_Ref
     (Self    : Connection_Factory)
     return MOMA.Types.Ref is
   begin
      return Self.Remote;
   end Get_Ref;

   -------------
   -- Set_Ref --
   -------------

   procedure Set_Ref
     (Self    : in out Connection_Factory;
      Remote  :        MOMA.Types.Ref) is
   begin
      Self.Remote := Remote;
   end Set_Ref;

end MOMA.Connection_Factories;
