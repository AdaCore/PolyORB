------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . P O L I C Y _ V A L U E S                   --
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

--  $Id$

package CORBA.Policy_Values is

   type Policy_Value is
     (ORB_CTRL_MODEL,
      SINGLE_THREAD_MODEL,
      MAIN_THREAD_MODEL,
      TRANSIENT,
      PERSISTENT,
      UNIQUE_ID,
      MULTIPLE_ID,
      USER_ID,
      SYSTEM_ID,
      IMPLICIT_ACTIVATION,
      NO_IMPLICIT_ACTIVATION,
      RETAIN,
      NON_RETAIN,
      USE_ACTIVE_OBJECT_MAP_ONLY,
      USE_DEFAULT_SERVANT,
      USE_SERVANT_MANAGER);

   subtype ThreadPolicyValue is Policy_Value
     range ORB_CTRL_MODEL .. MAIN_THREAD_MODEL;
   subtype LifespanPolicyValue is Policy_Value
     range TRANSIENT .. PERSISTENT;
   subtype IdUniquenessPolicyValue is Policy_Value
     range UNIQUE_ID .. MULTIPLE_ID;
   subtype IdAssignmentPolicyValue is Policy_Value
     range USER_ID .. SYSTEM_ID;
   subtype ImplicitActivationPolicyValue is Policy_Value
     range IMPLICIT_ACTIVATION .. NO_IMPLICIT_ACTIVATION;
   subtype ServantRetentionPolicyValue is Policy_Value
     range RETAIN .. NON_RETAIN;
   subtype RequestProcessingPolicyValue is Policy_Value
     range USE_ACTIVE_OBJECT_MAP_ONLY .. USE_SERVANT_MANAGER;

end CORBA.Policy_Values;
