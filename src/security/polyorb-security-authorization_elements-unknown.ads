------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.SECURITY.AUTHORIZATION_ELEMENTS.UNKNOWN              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Security.Types;

package PolyORB.Security.Authorization_Elements.Unknown is

   type Unknown_Authorization_Element_Type is
     new Authorization_Element_Type with private;

   overriding function Get_Authorization_Element_Type
     (Self : access Unknown_Authorization_Element_Type)
      return Element_Type;

   overriding function Is_Holder
     (Self     : access Unknown_Authorization_Element_Type;
      Identity :        PolyORB.Security.Identities.Identity_Access)
      return Boolean;

   overriding procedure Release_Contents
     (Self : access Unknown_Authorization_Element_Type);

   overriding function Encode
     (Self : access Unknown_Authorization_Element_Type)
      return Ada.Streams.Stream_Element_Array;

   function Create
     (The_Type : Element_Type;
      Contents : Ada.Streams.Stream_Element_Array)
      return Authorization_Element_Access;

private

   type Unknown_Authorization_Element_Type is
     new Authorization_Element_Type with
   record
      The_Type : Element_Type;
      The_Data : PolyORB.Security.Types.Stream_Element_Array_Access;
   end record;

end PolyORB.Security.Authorization_Elements.Unknown;
