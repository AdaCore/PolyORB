------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        C O R B A . R E Q U E S T                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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

with System;
with CORBA.AbstractBase;
with CORBA.NVList;
with CORBA.ExceptionList;
with CORBA.ContextList;
with CORBA.Context;

package CORBA.Request is

   type Object is private;

   procedure Add_Arg
     (Self      : in out Object;
      Arg_Type  : in     CORBA.TypeCode.Object;
      Value     : in     System.Address;
      Len       : in     Long;
      Arg_Flags : in     Flags);

   procedure Add_Arg
     (Self : in out Object;
      Arg  : in     NamedValue);

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : in     Flags  := 0);

   procedure Delete (Self : in out Object);

   procedure Send
     (Self         : in out Object;
      Invoke_Flags : in     Flags  := 0);

   procedure Get_Response
     (Self         : in out Object;
      Invoke_Flags : in     Flags  := 0);

   function Poll_Response (Self : in Object) return Boolean;

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

private

   type Object is
      record
         Ctx        : CORBA.Context.Ref;
         Target     : CORBA.AbstractBase.Ref;
         Operation  : CORBA.Identifier;
         Args_List  : CORBA.NVList.Ref;
         Result     : CORBA.NamedValue;
         Exc_List   : CORBA.ExceptionList.Ref;
         Ctxt_List  : CORBA.ContextList.Ref;
         Req_Flags  : CORBA.Flags;
      end record;

end CORBA.Request;
