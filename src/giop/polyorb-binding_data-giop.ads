------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . B I N D I N G _ D A T A . G I O P             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.GIOP_P.Tagged_Components;

package PolyORB.Binding_Data.GIOP is

   type GIOP_Profile_Type is abstract new Profile_Type with private;
   type GIOP_Profile_Factory is abstract new Profile_Factory with private;

   procedure Release (P : in out GIOP_Profile_Type);

   function Get_Component
     (P : in GIOP_Profile_Type;
      C : in PolyORB.GIOP_P.Tagged_Components.Tag_Value)
      return PolyORB.GIOP_P.Tagged_Components.Tagged_Component_Access;

private

   type GIOP_Profile_Type is abstract new Profile_Type with record
      Version_Major : Types.Octet;
      Version_Minor : Types.Octet;

      --  Tagged components list

      Components    : PolyORB.GIOP_P.Tagged_Components.Tagged_Component_List;
   end record;

   type GIOP_Profile_Factory is abstract new Profile_Factory with null record;

end PolyORB.Binding_Data.GIOP;
