------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        C O R B A . R E Q U E S T                         --
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

--  The CORBA Dynamic Invocation Interface.

--  $Id$

with Ada.Finalization;

with CORBA.AbstractBase;
with CORBA.Context;
with CORBA.ContextList;
with CORBA.ExceptionList;
with CORBA.NVList;

with PolyORB.Requests;

package CORBA.Request is

   --  pragma Elaborate_Body;

   type Object is limited private;

   procedure Create_Request
     (Self      : in     CORBA.AbstractBase.Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags);

   procedure Create_Request
     (Self      : in     CORBA.AbstractBase.Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Exc_List  : in     ExceptionList.Ref;
      Ctxt_List : in     ContextList.Ref;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags);

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : in     Flags  := 0);

   procedure Delete (Self : in out Object);

   --  XXX incomplete!

   --  The following is specific to PolyORB.

   function To_PolyORB_Request
     (Request : Object)
     return PolyORB.Requests.Request_Access;

private

   type Object is new Ada.Finalization.Limited_Controlled with record
      The_Request : PolyORB.Requests.Request_Access;
   end record;
   --  XXX Would it not be simpler to declare
   --  type Object is new PolyORB.Requests.Request_Access; ?
   --  (as is presently done in CORBA.ServerRequest!)

   procedure Finalize (X : in out Object);

   pragma Inline (To_PolyORB_Request);

end CORBA.Request;

