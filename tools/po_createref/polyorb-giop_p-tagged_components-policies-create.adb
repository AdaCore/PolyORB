------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.GIOP_P.TAGGED_COMPONENTS.POLICIES.CREATE              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2007, Free Software Foundation, Inc.             --
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

with PolyORB.Types;
with PolyORB.GIOP_P.Tagged_Components.Policies.Priority_Model_Policy;
with PolyORB.Representations.CDR.Common;

package body PolyORB.GIOP_P.Tagged_Components.Policies.Create is

   use PolyORB.GIOP_P.Tagged_Components.Policies;
   use PolyORB.GIOP_P.Tagged_Components.Policies.Priority_Model_Policy;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Types;
   use Policy_Value_Seq;

   procedure Create_TC
     (Param      : Parameter_Component;
      TC         : in out TC_Policies;
      Error      : out Boolean)
   is
      Buffer      : Buffer_Access := new Buffer_Type;
      Policy_V    : Policy_Value;
      Model       : Natural;
   begin
      --  Policy component BNF :
      --  -pol_nb <Policies_number> {Policies}

      --  Policy subcomponent BNF :
      --  -model <Priority_Model>  -priority <External_Priority>
      --  where <Priority_Model> := CLIENT|SERVER_DECLARED

      Error := False;

      for J in Param.Policies.all'Range loop

         if Param.Policies.all (J).Priority_Model.all = "CLIENT" then
            Model := 0;
         elsif Param.Policies.all (J).Priority_Model.all =
           "SERVER_DECLARED" then
            Model := 1;
         else
            Error := True;
            return;
         end if;

         --  Create a Policy value
         Start_Encapsulation (Buffer);
         Marshall (Buffer,
                   PolyORB.Types.Unsigned_Long (Model));
         Marshall (Buffer, PolyORB.Types.Short
                   (Param.Policies.all (J).Priority_Value));
         Policy_V :=
           Policy_Value'(P_Type => 40,
                         P_Value =>
                           new Encapsulation'(Encapsulate (Buffer)));
         Release (Buffer);

         --  add policy value to the tagged component
         Append (TC.Policies, Policy_V);
      end loop;
   end Create_TC;

end PolyORB.GIOP_P.Tagged_Components.Policies.Create;
