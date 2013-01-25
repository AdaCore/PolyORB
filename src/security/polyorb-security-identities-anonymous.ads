------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SECURITY.IDENTITIES.ANONYMOUS                   --
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

pragma Ada_2005;

package PolyORB.Security.Identities.Anonymous is

   type Anonymous_Identity_Type is new Identity_Type with null record;

   function Create_Anonymous_Identity return Identity_Access;

private

   --  Derived from Identity_Token_Type

   overriding function Get_Token_Type
     (Self : access Anonymous_Identity_Type)
      return PolyORB.Security.Types.Identity_Token_Type;

   overriding function Get_Printable_Name
     (Self : access Anonymous_Identity_Type)
      return String;

   overriding function Duplicate
     (Self : access Anonymous_Identity_Type)
      return Identity_Access;

   overriding procedure Release_Contents
     (Self : access Anonymous_Identity_Type);

   overriding function Encode
     (Self : access Anonymous_Identity_Type)
      return Ada.Streams.Stream_Element_Array;

   overriding procedure Decode
     (Self  : access Anonymous_Identity_Type;
      Item  :        Ada.Streams.Stream_Element_Array;
      Error : in out PolyORB.Errors.Error_Container);

end PolyORB.Security.Identities.Anonymous;
