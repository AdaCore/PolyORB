------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.SECURITY.AUTHORIZATION_ELEMENTS                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with Ada.Streams;

with PolyORB.Security.Identities;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.Security.Authorization_Elements is

   type Element_Type is new PolyORB.Types.Unsigned_Long;

   type Authorization_Element_Type is abstract tagged null record;

   type Authorization_Element_Access is
     access all Authorization_Element_Type'Class;

   function Get_Authorization_Element_Type
     (Self : access Authorization_Element_Type)
      return Element_Type
      is abstract;

   function Is_Holder
     (Self     : access Authorization_Element_Type;
      Identity :        PolyORB.Security.Identities.Identity_Access)
      return Boolean
      is abstract;

   procedure Release_Contents
     (Self : access Authorization_Element_Type)
      is abstract;

   function Encode
     (Self : access Authorization_Element_Type)
      return Ada.Streams.Stream_Element_Array
      is abstract;

   package Authorization_Element_Lists is
     new PolyORB.Utils.Chained_Lists (Authorization_Element_Access);

   procedure Release_Contents (Item : in out Authorization_Element_Lists.List);

   function Create
     (The_Type : Element_Type;
      Contents : Ada.Streams.Stream_Element_Array)
      return Authorization_Element_Access;

private

   type Element_Constructor is
     access function
     (Contents : Ada.Streams.Stream_Element_Array)
      return Authorization_Element_Access;

   procedure Register
     (The_Type    : Element_Type;
      Constructor : Element_Constructor);

end PolyORB.Security.Authorization_Elements;
