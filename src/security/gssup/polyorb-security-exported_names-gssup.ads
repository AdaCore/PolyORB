------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SECURITY.EXPORTED_NAMES.GSSUP                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

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

   function Is_Equivalent
     (Left  : access GSSUP_Exported_Name_Type;
      Right : access Exported_Name_Type'Class)
      return Boolean;

   function Get_Printable_Name
     (Item : access GSSUP_Exported_Name_Type)
      return String;

   function Duplicate
     (Item : access GSSUP_Exported_Name_Type)
      return Exported_Name_Access;

   procedure Release_Contents (Item : access GSSUP_Exported_Name_Type);

   function Encode_Name_BLOB
     (Item : access GSSUP_Exported_Name_Type)
      return Ada.Streams.Stream_Element_Array;

   procedure Decode_Name_BLOB
     (Item  : access GSSUP_Exported_Name_Type;
      BLOB  :        Ada.Streams.Stream_Element_Array;
      Error : in out PolyORB.Errors.Error_Container);

end PolyORB.Security.Exported_Names.GSSUP;
