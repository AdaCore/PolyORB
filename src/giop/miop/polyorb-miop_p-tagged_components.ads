------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . M I O P _ P . T A G G E D _ C O M P O N E N T S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2006 Free Software Foundation, Inc.           --
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

--  MIOP specific tagged components

with PolyORB.Buffers;
with PolyORB.Errors;
with PolyORB.GIOP_P.Tagged_Components;
with PolyORB.MIOP_P.Groups;
with PolyORB.Types;

package PolyORB.MIOP_P.Tagged_Components is

   use PolyORB.Buffers;
   use PolyORB.GIOP_P.Tagged_Components;
   use PolyORB.MIOP_P.Groups;

   TC_Group_Info_Version_Major : constant Types.Octet;
   TC_Group_Info_Version_Minor : constant Types.Octet;

   type TC_Group_Info is new Tagged_Component
     (Tag => Tag_Group, At_Most_Once => False)
     with record
        G_I : aliased Group_Info;
     end record;
   type TC_Group_Info_Access is access all TC_Group_Info;

   procedure Marshall_Component_Data
     (Comp   : access TC_Group_Info;
      Buffer : access Buffer_Type);

   procedure Unmarshall_Component_Data
     (Comp   : access TC_Group_Info;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container);

   procedure Release_Contents (Comp : access TC_Group_Info);

   function Duplicate (Comp : TC_Group_Info) return Tagged_Component_Access;

   function To_String (Comp : access TC_Group_Info) return String;
   --  Convert C into an element of a corbaloc

   function From_String (S : String) return TC_Group_Info_Access;
   --  Convert S into a TC_Group_Info_Access.
   --  S must follow corbaloc syntax.

private

   TC_Group_Info_Version_Major : constant Types.Octet := 1;
   TC_Group_Info_Version_Minor : constant Types.Octet := 0;

end PolyORB.MIOP_P.Tagged_Components;
