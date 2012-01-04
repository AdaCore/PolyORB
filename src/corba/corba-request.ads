------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        C O R B A . R E Q U E S T                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

--  The CORBA Dynamic Invocation Interface.

with CORBA.AbstractBase;
with CORBA.Context;
with CORBA.ContextList;
with CORBA.ExceptionList;
with CORBA.NVList;

with PolyORB.Requests;

package CORBA.Request is

   type Object is limited private;

   procedure Create_Request
     (Self      : CORBA.AbstractBase.Ref;
      Ctx       : CORBA.Context.Ref;
      Operation : Identifier;
      Arg_List  : CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   :    out CORBA.Request.Object;
      Req_Flags : Flags);

   procedure Create_Request
     (Self      : CORBA.AbstractBase.Ref;
      Ctx       : CORBA.Context.Ref;
      Operation : Identifier;
      Arg_List  : CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Exc_List  : ExceptionList.Ref;
      Ctxt_List : ContextList.Ref;
      Request   :    out CORBA.Request.Object;
      Req_Flags : Flags);

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : Flags  := 0);
   --  Implementation Note: the IDL-to-Ada mapping specifies a default
   --  value for Invoke_Flags, but it does not define its
   --  semantics. Moreover, the CORBA specifications define no value
   --  for Invoke_Flags. Thus, we retain the following semantics: the
   --  only possible value for Invoke_Flags is 0, all other values
   --  will be ignored for now.

   procedure Delete (Self : in out Object);

   --  XXX incomplete!

private

   type Object is limited record
      The_Request : aliased PolyORB.Requests.Request;
   end record;
   --  XXX Would it not be simpler to declare
   --  type Object is new PolyORB.Requests.Request; ?
   --  (as is presently done in CORBA.ServerRequest!)

end CORBA.Request;
