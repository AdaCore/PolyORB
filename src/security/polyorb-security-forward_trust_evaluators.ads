------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.SECURITY.FORWARD_TRUST_EVALUATORS                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Security.Authorization_Elements;
with PolyORB.Security.Identities;

package PolyORB.Security.Forward_Trust_Evaluators is

   type Forward_Trust_Evaluator is abstract tagged null record;

   type Forward_Trust_Evaluator_Access is
     access all Forward_Trust_Evaluator'Class;

   procedure Evaluate_Trust
     (Evaluator           : access Forward_Trust_Evaluator;
      Target_Identity     :        PolyORB.Security.Identities.Identity_Access;
      Client_Identity     :        PolyORB.Security.Identities.Identity_Access;
      Authorization_Token :
        PolyORB.Security.Authorization_Elements.
          Authorization_Element_Lists.List;
      Delegation_Required :        Boolean;
      No_Information      :    out Boolean;
      Trusted             :    out Boolean)
      is abstract;

end PolyORB.Security.Forward_Trust_Evaluators;
