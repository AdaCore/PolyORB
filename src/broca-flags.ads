------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                          B R O C A . F L A G S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA;

package Broca.Flags is
   --  The body is in charge to decode options during elaboration.
   pragma Elaborate_Body (Broca.Flags);

   --  Not highly used.
   Verbose : Boolean := True;

   --  If TRUE, server logs incoming requests, processing...
   --  Can be enable by -ORBlog
   Log : Boolean := False;

   --  TCP/IP port used.
   --  -ORBport N
   Port : Natural := 0;

   --  Number of tasks used by the server.
   --  If 1, it use the main task (ie, the task that has called run) to do all
   --  the work.  If greather than 1, it creates tasks.
   --  -ORBserver-tasks N
   Nbr_Server_Tasks : Positive := 1;

   --  Maximum number of consecutive location forward.
   Max_Tries : Natural := 2;

   --  A coded value of time when the server was started up.  This is used in
   --  ObjectId.
   Boot_Time : CORBA.Unsigned_Long;
end Broca.Flags;
