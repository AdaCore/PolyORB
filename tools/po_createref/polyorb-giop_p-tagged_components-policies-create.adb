------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.GIOP_P.TAGGED_COMPONENTS.POLICIES.CREATE              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2013, Free Software Foundation, Inc.          --
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
           "SERVER_DECLARED"
         then
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
