------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        C O R B A . R E Q U E S T                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.7 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
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

with CORBA.NVList;
with AdaBroker.OmniORB;


package CORBA.Request is

   type Object is private;

   procedure Add_Arg
     (Self : in out Object;
      Arg  : in     NamedValue);
   --  add an argument to the list of arguments for the operation

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : in     Flags); --  no legal value listed in the spec !
   --  basic invocation

   procedure Delete
     (Self : in out Object);
   --  delete the request : what does it mean ?

   procedure Send
     (Self         : in out Object;
      Invoke_Flags : in     Flags);
   --  incation, returns control to the caller without waiting for the
   --  operation to finish
   --  no fully implemented

   procedure Get_Response
     (Self           : in out Object;
      Response_Flags : in     Flags);
   --  not implementaed : to be used with send

   --  implementation defined

   procedure Set
     (Self       :    out CORBA.Request.Object;
      OmniObj    : in     AdaBroker.OmniORB.OmniObject_Ptr;
      Operation  : in     CORBA.Identifier;
      Arg_List   : in     CORBA.NVList.Object;
      Result     : in     CORBA.NamedValue;
      Req_Flags  : in     CORBA.Flags;
      Returns    : in     Status);

   function Return_Value
     (Self : in CORBA.Request.Object)
     return CORBA.NamedValue;
   --  to get the return value when operation was a function

   function Return_Arguments
     (Self : in CORBA.Request.Object)
      return CORBA.NVList.Object;
   --  to get the arguments when operation  was a procedure


private
   --  implementation defined

   type Object is
      record
         Target     : AdaBroker.OmniORB.OmniObject_Ptr;
         Operation  : CORBA.Identifier;
         Args_List  : CORBA.NVList.Object;
         Result     : CORBA.NamedValue;
         Req_Flags  : CORBA.Flags;
         Returns    : CORBA.Status;
      end record;

end CORBA.Request;
