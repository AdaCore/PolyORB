------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . D Y N A M I C _ D I C T                  --
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

--  A dynamic dictionnary of objects, indexed by Strings.

generic

   type Value is private;

package PolyORB.Dynamic_Dict is

   pragma Preelaborate;

   procedure Register
     (K : String;
      V : Value);
   --  Associate key K with value V.

   procedure Unregister
     (K : String);
   --  Remove any association for K.

   function Lookup
     (K       : String;
      Default : Value)
     return Value;
   --  Lookup K in the dictionary, and return the associated value.
   --  Default is returned for non-registered keys.

   procedure Reset;
   --  Remove all key associations

   type Dict_Action is access procedure (K : String; V : Value);
   procedure For_Each (Action : Dict_Action);
   --  Execute Action for each association that exists in the dictionary

end PolyORB.Dynamic_Dict;
