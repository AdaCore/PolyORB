------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.SECURITY.IDENTITIES.PRINCIPAL_NAME                 --
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

with PolyORB.Security.Exported_Names;

package PolyORB.Security.Identities.Principal_Name is

   type Principal_Name_Identity_Type is new Identity_Type with private;

   function Create_Principal_Name_Identity
     (Principal_Name : PolyORB.Security.Exported_Names.Exported_Name_Access)
      return Identity_Access;

private

   type Principal_Name_Identity_Type is new Identity_Type with record
      Principal_Name : PolyORB.Security.Exported_Names.Exported_Name_Access;
   end record;

   --  Derived from Identity_Token_Type

   function Get_Token_Type
     (Self : access Principal_Name_Identity_Type)
      return PolyORB.Security.Types.Identity_Token_Type;

   function Get_Printable_Name
     (Self : access Principal_Name_Identity_Type)
      return String;

   function Duplicate
     (Self : access Principal_Name_Identity_Type)
      return Identity_Access;

   procedure Release_Contents
     (Self : access Principal_Name_Identity_Type);

   function Encode
     (Self : access Principal_Name_Identity_Type)
      return Ada.Streams.Stream_Element_Array;

   procedure Decode
     (Self  : access Principal_Name_Identity_Type;
      Item  :        Ada.Streams.Stream_Element_Array;
      Error : in out PolyORB.Errors.Error_Container);

end PolyORB.Security.Identities.Principal_Name;
