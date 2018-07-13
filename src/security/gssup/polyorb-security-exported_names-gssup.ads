------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SECURITY.EXPORTED_NAMES.GSSUP                   --
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

pragma Ada_2012;

--  This package provide support for GSSUP mechanism Exported Name

package PolyORB.Security.Exported_Names.GSSUP is

   type GSSUP_Exported_Name_Type is new Exported_Name_Type with private;

   type GSSUP_Exported_Name_Access is
     access all GSSUP_Exported_Name_Type'Class;

--   function Get_Scoped_Name
--     (Item : access GSSUP_Exported_Name_Type)
--      return String;

   function Create_GSSUP_Exported_Name
     (Scoped_Name : String)
      return Exported_Name_Access;

private

   type String_Access is access all String;

   type GSSUP_Exported_Name_Type is new Exported_Name_Type with record
      Scoped_Name : String_Access := null;
   end record;

   --  Derived from Exported_Name_Type

   overriding function Is_Equivalent
     (Left  : access GSSUP_Exported_Name_Type;
      Right : access Exported_Name_Type'Class)
      return Boolean;

   overriding function Get_Printable_Name
     (Item : access GSSUP_Exported_Name_Type)
      return String;

   overriding function Duplicate
     (Item : access GSSUP_Exported_Name_Type)
      return Exported_Name_Access;

   overriding procedure Release_Contents
     (Item : access GSSUP_Exported_Name_Type);

   overriding function Encode_Name_BLOB
     (Item : access GSSUP_Exported_Name_Type)
      return Ada.Streams.Stream_Element_Array;

   overriding procedure Decode_Name_BLOB
     (Item  : access GSSUP_Exported_Name_Type;
      BLOB  :        Ada.Streams.Stream_Element_Array;
      Error : in out PolyORB.Errors.Error_Container);

end PolyORB.Security.Exported_Names.GSSUP;
