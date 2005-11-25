------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.GIOP_P.TAGGED_COMPONENTS.POLICIES.PRINT              --
--                                                                          --
--                                 B o d y                                  --
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

with Output;

with PolyORB.Representations.CDR.Common;
with PolyORB.Types;
with PolyORB.Utils;

package body PolyORB.GIOP_P.Tagged_Components.Policies.Print is

   ---------------
   -- Output_TC --
   ---------------

   procedure Output_TC (TC : TC_Policies) is
      use Policy_Value_Seq;

      use Output;

      use PolyORB.Representations.CDR.Common;
      use PolyORB.Types;
      use PolyORB.Utils;

   begin
      Inc_Indent;
      Put_Line ("Length", Length (TC.Policies)'Img);

      declare
         It : Policy_Value_Seq.Iterator
           := First (TC.Policies);
         P_Type : PolyORB.Types.Unsigned_Long;
         Counter : Natural := 1;

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

end PolyORB.GIOP_P.Tagged_Components.Policies.Print;
