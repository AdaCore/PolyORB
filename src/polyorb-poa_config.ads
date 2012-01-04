------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . P O A _ C O N F I G                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Global POA configuration

with PolyORB.POA_Policies;

package PolyORB.POA_Config is

   pragma Elaborate_Body;

   type Configuration_Type is abstract tagged limited private;
   type Configuration_Access is access all Configuration_Type'Class;

   procedure Initialize (C : Configuration_Type) is abstract;
   --  Create all policies available in this configuration,
   --  and register them with policy repository F.

   function Default_Policies
     (C : Configuration_Type)
      return PolyORB.POA_Policies.PolicyList is abstract;
   --  Return the list of default OA policies for this configuration

   procedure Set_Configuration (C : Configuration_Access);
   --  Set the configuration for the whole runtime.
   --  May be called only once. C must be non-null.

   function Configuration return Configuration_Access;
   --  The value set by Set_Configuration

private

   type Configuration_Type is abstract tagged limited null record;

   pragma Inline (Set_Configuration);
   pragma Inline (Configuration);

end PolyORB.POA_Config;
