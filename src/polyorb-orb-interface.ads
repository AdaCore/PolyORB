------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . O R B . I N T E R F A C E                 --
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

--  The messages supported by ORBs (middleware core module).

--  $Id$

with PolyORB.Components;
with PolyORB.Jobs;
with PolyORB.Requests;
with PolyORB.Types;

package PolyORB.ORB.Interface is

   type Queue_Job is new Components.Message with record
      Job : PolyORB.Jobs.Job_Access;
   end record;
   --  Queue Job for execution by the receiving ORB.
   --  No reply (the job will be executed asynchronously).

   type Queue_Request is new Components.Message with record
      Request   : Requests.Request_Access;
      Requestor : Components.Component_Access;
      --  Requesting_Task : Task_Info.Task_Info_Access;
   end record;
   --  Queue method invocation request Req for execution by Server
   --  on behalf of a remote caller. No reply expected.
   --  When the request is executed, a message will be sent
   --  back to Requestor (asynchronously). If this request comes
   --  from a Session, Requestor must be set to that Session.
   --  If the request is submitted directly by a local client task,
   --  Requestor must be set to null.
   --  The client the responsible of the destruction of
   --  the Request after its execution is completed.

   type Oid_Translate is new Components.Message with record
      Oid : Objects.Object_Id_Access;
   end record;

   type URI_Translate is new Components.Message with record
      Path : Types.String;
   end record;

end PolyORB.ORB.Interface;
