------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   C O R B A . P O L I C Y _ T Y P E S                    --
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

package CORBA.Policy_Types is

   --  Values from the standard IDL for module PortableServer.
   --  XXX These belong in package PortableServer -- there should
   --      not be a package CORBA.Policy_Types!

   THREAD_POLICY_ID               : constant PolicyType := 16;
   LIFESPAN_POLICY_ID             : constant PolicyType := 17;
   ID_UNIQUENESS_POLICY_ID        : constant PolicyType := 18;
   ID_ASSIGNMENT_POLICY_ID        : constant PolicyType := 19;
   IMPLICIT_ACTIVATION_POLICY_ID  : constant PolicyType := 20;
   SERVANT_RETENTION_POLICY_ID    : constant PolicyType := 21;
   REQUEST_PROCESSING_POLICY_ID   : constant PolicyType := 22;

end CORBA.Policy_Types;
