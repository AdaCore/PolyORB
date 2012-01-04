------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.SECURITY.FORWARD_TRUST_EVALUATORS                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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
