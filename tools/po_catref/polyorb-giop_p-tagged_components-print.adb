------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.GIOP_P.TAGGED_COMPONENTS.PRINT                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 1-2004 Free Software Foundation, Inc.            --
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

with Output;

with PolyORB.GIOP_P.Tagged_Components.Policies.Print;

with PolyORB.Utils;

package body PolyORB.GIOP_P.Tagged_Components.Print is

   ------------------------------
   -- Output_Tagged_Components --
   ------------------------------

   procedure Output_Tagged_Components
     (TCs : PolyORB.GIOP_P.Tagged_Components.Tagged_Component_List)
   is
      use Output;

      use PolyORB.GIOP_P.Tagged_Components.Policies.Print;
      use PolyORB.GIOP_P.Tagged_Components.Policies;

      use PolyORB.Utils;

      TC : Tagged_Component_Access;

      It : Iterator := First (TCs);
      J : Natural := 1;
   begin
      Put_Line ("Tagged components",
                Integer'Image (Length (TCs)));

      while not Last (It) loop
         TC := Value (It).all;
         Inc_Indent;

         Put_Line ("Component #" & Trimmed_Image (J), "");
         J := J + 1;

         Inc_Indent;

         Put_Line ("Tag", TC.Tag'Img);

         if TC.Tag = Tag_Policies then
            Put_Line ("Type", "TAG_Policies");
            Output_TC (TC_Policies (TC.all));

         elsif TC.Tag = Tag_Group then
            Put_Line ("Type", "TAG_Group");

         end if;

         Dec_Indent;

         Dec_Indent;
         Next (It);
      end loop;
   end Output_Tagged_Components;

end PolyORB.GIOP_P.Tagged_Components.Print;
