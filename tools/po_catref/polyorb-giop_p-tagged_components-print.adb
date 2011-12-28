------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.GIOP_P.TAGGED_COMPONENTS.PRINT                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2011, Free Software Foundation, Inc.         --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps;

with Output;

with PolyORB.Types;

package body PolyORB.GIOP_P.Tagged_Components.Print is

   function Hash (Item : Tag_Value) return Ada.Containers.Hash_Type;

   package Tag_Maps is
     new Ada.Containers.Hashed_Maps (Tag_Value, Output_Procedure, Hash, "=");

   Registry : Tag_Maps.Map;

   ----------
   -- Hash --
   ----------

   function Hash (Item : Tag_Value) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Item);
   end Hash;

   ------------------------------
   -- Output_Tagged_Components --
   ------------------------------

   procedure Output_Tagged_Components
     (TCs : PolyORB.GIOP_P.Tagged_Components.Tagged_Component_List)
   is
      use Output;

      use PolyORB.Utils;
      use PolyORB.Types;

      TC : Tagged_Component_Access;

      It : Iterator := First (TCs);
      J : Long_Long := 1;
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

         if Registry.Contains (TC.Tag) then
            Registry.Element (TC.Tag) (TC.all);

         else
            case TC.Tag is
               when Tag_SSL_Sec_Trans =>
                  Put_Line ("Type", "TAG_SSL_Sec_Trans");

               when Tag_Group =>
                  Put_Line ("Type", "TAG_Group");

               when others =>
                  null;
            end case;
         end if;

         Dec_Indent;

         Dec_Indent;
         Next (It);
      end loop;
   end Output_Tagged_Components;

   --------------
   -- Register --
   --------------

   procedure Register (Tag : Tag_Value; Output : Output_Procedure) is
   begin
      Registry.Insert (Tag, Output);
   end Register;

end PolyORB.GIOP_P.Tagged_Components.Print;
