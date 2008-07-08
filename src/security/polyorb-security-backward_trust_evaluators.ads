------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.SECURITY.BACKWARD_TRUST_EVALUATORS                 --
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

with PolyORB.Security.Identities;
with PolyORB.Types;

package PolyORB.Security.Backward_Trust_Evaluators is

   type Backward_Trust_Evaluator is tagged private;

   type Backward_Trust_Evaluator_Access is
     access all Backward_Trust_Evaluator'Class;

   procedure Evaluate_Trust
     (Evaluator         : access Backward_Trust_Evaluator;
      Client_Identity   :        PolyORB.Security.Identities.Identity_Access;
      Asserted_Identity :        PolyORB.Security.Identities.Identity_Access;
      Trusted           :    out Boolean);

   function Create_Backward_Trust_Evaluator
     (File : String) return Backward_Trust_Evaluator_Access;

private

   type Backward_Trust_Evaluator is tagged record
      File_Name : PolyORB.Types.String;
   end record;

end PolyORB.Security.Backward_Trust_Evaluators;
