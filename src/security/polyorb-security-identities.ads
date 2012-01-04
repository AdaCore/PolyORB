------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . S E C U R I T Y . I D E N T I T I E S           --
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

with PolyORB.Errors;
with PolyORB.Security.Types;

package PolyORB.Security.Identities is

   type Identity_Type is abstract tagged null record;

   type Identity_Access is access all Identity_Type'Class;

   function Get_Token_Type
     (Self : access Identity_Type)
      return PolyORB.Security.Types.Identity_Token_Type
      is abstract;

   function Get_Printable_Name
     (Self : access Identity_Type)
      return String
      is abstract;

   function Duplicate (Self : access Identity_Type) return Identity_Access
      is abstract;

   procedure Release_Contents (Self : access Identity_Type) is abstract;

   procedure Destroy (Item : in out Identity_Access);

   function Encode
     (Self : access Identity_Type)
      return Ada.Streams.Stream_Element_Array
      is abstract;

   procedure Decode
     (Self  : access Identity_Type;
      Item  :        Ada.Streams.Stream_Element_Array;
      Error : in out PolyORB.Errors.Error_Container)
      is abstract;

   procedure Create
     (Kind  :        PolyORB.Security.Types.Identity_Token_Type;
      Item  :        Ada.Streams.Stream_Element_Array;
      Token :    out Identity_Access;
      Error : in out PolyORB.Errors.Error_Container);

private

   type Empty_Identity_Constructor is
     access function return Identity_Access;

   procedure Register
     (Kind        : PolyORB.Security.Types.Identity_Token_Type;
      Constructor : Empty_Identity_Constructor);

end PolyORB.Security.Identities;
