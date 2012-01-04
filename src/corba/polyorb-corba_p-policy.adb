------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . C O R B A _ P . P O L I C Y                --
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

package body PolyORB.CORBA_P.Policy is

   ---------------------
   -- Get_Policy_Type --
   ---------------------

   function Get_Policy_Type
     (Self : Policy_Object_Type)
     return CORBA.PolicyType is
   begin
      return Self.Policy;
   end Get_Policy_Type;

   ---------------------
   -- Set_Policy_Type --
   ---------------------

   procedure Set_Policy_Type
     (Self   : in out Policy_Object_Type;
      Policy :        CORBA.PolicyType) is
   begin
      Self.Policy := Policy;
   end Set_Policy_Type;

   ----------------------
   -- Get_Policy_Value --
   ----------------------

   function Get_Policy_Value
     (Self : Policy_Object_Type)
     return CORBA.Any is
   begin
      return Self.Value;
   end Get_Policy_Value;

   ----------------------
   -- Set_Policy_Value --
   ----------------------

   procedure Set_Policy_Value
     (Self  : in out Policy_Object_Type;
      Value :        CORBA.Any) is
   begin
      Self.Value := Value;
   end Set_Policy_Value;

end PolyORB.CORBA_P.Policy;
