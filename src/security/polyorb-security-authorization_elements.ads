------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.SECURITY.AUTHORIZATION_ELEMENTS                  --
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
