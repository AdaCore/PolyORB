------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.GIOP_P.TAGGED_COMPONENTS.POLICIES.PRINT              --
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

with PO_Catref.Output;

with PolyORB.GIOP_P.Tagged_Components.Print;
with PolyORB.Initialization;
with PolyORB.Representations.CDR.Common;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Tagged_Components.Policies.Print is

   procedure Output (Item : Tagged_Component'Class);

   procedure Output_TC (TC : TC_Policies);

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PolyORB.GIOP_P.Tagged_Components.Print.Register
       (Tag_Policies, Output'Access);
   end Initialize;

   ------------
   -- Output --
   ------------

   procedure Output (Item : Tagged_Component'Class) is
   begin
      Output_TC (TC_Policies (Item));
   end Output;

   ---------------
   -- Output_TC --
   ---------------

   procedure Output_TC (TC : TC_Policies) is
      use PO_Catref.Output;
      use Policy_Value_Seq;
      use PolyORB.Representations.CDR.Common;
      use PolyORB.Types;

   begin
      Put_Line ("Type", "TAG_Policies");
      Inc_Indent;
      Put_Line ("Length", Length (TC.Policies)'Img);

      declare
         It : Policy_Value_Seq.Iterator
           := First (TC.Policies);
         P_Type : PolyORB.Types.Unsigned_Long;
         Counter : Long_Long := 1;

      begin
         Inc_Indent;

         while not Last (It) loop
            P_Type := Value (It).P_Type;

            if P_Type = 40 then
               Put_Line ("Policy #" & Trimmed_Image (Counter),
                         "Priority_Model_Policy_Type (40)");

               declare
                  Buffer : aliased Buffer_Type;
                  Model : PolyORB.Types.Unsigned_Long;
                  Priority : PolyORB.Types.Short;
               begin
                  Decapsulate (Value (It).P_Value, Buffer'Access);

                  Model := Unmarshall (Buffer'Access);
                  if Model = 0 then
                     Put_Line ("Model", "CLIENT");
                  elsif Model = 1 then
                     Put_Line ("Model", "SERVER_DECLARED");
                  else
                     Put_Line ("Inconsistent value", Model'Img);
                  end if;

                  Priority := Unmarshall (Buffer'Access);
                  Put_Line ("Priority", Priority'Img);
               end;

            end if;

            Next (It);
            Counter := Counter + 1;
         end loop;

         Dec_Indent;
      end;

      Dec_Indent;
   end Output_TC;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tagged_components.policies.print",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => +"tagged_components.policies",
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.GIOP_P.Tagged_Components.Policies.Print;
