------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . S E R V A N T S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Root type for concrete object implementations (servants).

--  $Id$

with PolyORB.Components;
with PolyORB.POA_Policies.Thread_Policy;

package PolyORB.Servants is

   type Servant is abstract new PolyORB.Components.Component with private;

   type Servant_Access is access all Servant'Class;
   --  A Servant is a Component that supports the messages
   --  defined in PolyORB.Objects.Interface. This type may
   --  be further derived by personality-specific units.

   function Handle_Message
     (S   : access Servant;
      Msg : Components.Message'Class)
      return Components.Message'Class is abstract;

   procedure Set_Thread_Policy
     (S  : access Servant;
      TP : POA_Policies.Thread_Policy.ThreadPolicy_Access);
   --  Set a ThreadPolicy pointer for the servant

   pragma Inline (Set_Thread_Policy);

private

   type Servant is abstract new PolyORB.Components.Component
     with record
        TP_Access : POA_Policies.Thread_Policy.ThreadPolicy_Access := null;
     end record;
   --  TP_Access is a ThreadPolicy_Access. It is a pointer to the
   --  ThreadPolicy of the OA that manages the Servant. If the OA is not
   --  a POA, then this policy will be the ORB_CTRL_MODEL policy. This field
   --  is needed by PolyORB in order to execute a request on a servant with
   --  the appropriate Thread Policy. We can notice that only the POAs use
   --  different thread policies, but as the ORB doesn't know if the OA is
   --  a POA or not, a generic mechanism is needed.
   --  1) When a Servant is created, TP_Access is set to null;
   --  2) When the ORB has to execute a job, it executes PolyORB.Jobs.Run
   --  3) In Jobs.Run, a surrogate is binded to an object (servant, session,..)
   --     with the References-Binding.Bind function
   --  4) If the object is a Servant, its Thread_Policy will be set
   --  5) Then, after the binding, a message will be sent to the servant
   --  6) The servant will use TP_Access in order to be executed with
   --     the appropriate thread policy (POA case) or with the ORB_CTRL_MODEL
   --     (other cases)

end PolyORB.Servants;
