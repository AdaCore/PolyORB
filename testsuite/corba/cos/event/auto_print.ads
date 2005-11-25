------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           A U T O _ P R I N T                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

with CosEventComm;

with CORBA.Impl;

with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;

package Auto_Print is

   use CosEventComm;

   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;

   procedure Auto_Display;

   procedure Ensure_Initialization;
   pragma Inline (Ensure_Initialization);
   --  Ensure that the Mutexes are initialized

   Session_Mutex : Mutex_Access;
   Session_Taken : Condition_Access;
   --  Synchornisation of task initialization.

   EndDisplay : Boolean := False;

   A_S : CORBA.Impl.Object_Ptr := null;
   --  This variable is used to initialize the threads local variable.
   --  it is used to replace the 'accept' statement.

end Auto_Print;
