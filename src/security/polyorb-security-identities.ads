------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . S E C U R I T Y . I D E N T I T I E S           --
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
