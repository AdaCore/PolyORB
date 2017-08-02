------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.GIOP_P.TAGGED_COMPONENTS.PRINT                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with Ada.Containers.Hashed_Maps;

with PO_Catref.Output;

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
      use PO_Catref.Output;
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
