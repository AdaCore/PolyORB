------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . M I O P _ P . T A G G E D _ C O M P O N E N T S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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
