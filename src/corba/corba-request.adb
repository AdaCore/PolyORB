------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        C O R B A . R E Q U E S T                         --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.Requests;

with CORBA.Context;
with CORBA.NVList;
with CORBA.Object;

package body CORBA.Request is

   procedure Create_Request
     (Self      : in     CORBA.AbstractBase.Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   :    out Object;
      Req_Flags : in     Flags)
   is
      PResult : PolyORB.Any.NamedValue;
      for PResult'Address use Result'Address;
      pragma Import (Ada, PResult);
      --  This is ugly but required because we want Result
      --  to be strictly passed by reference, with no intervening
      --  assignments.
   begin
      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (CORBA.AbstractBase.Ref'Class (Self))),
         Operation => To_Standard_String (Operation),
         Arg_List  => CORBA.NVList.To_PolyORB_Ref (Arg_List),
         Result    => PResult,
         Req       => Request.The_Request);
   end Create_Request;

   procedure Create_Request
     (Self      : in     CORBA.AbstractBase.Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Exc_List  : in     ExceptionList.Ref;
      Ctxt_List : in     ContextList.Ref;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags) is
   begin
      raise PolyORB.Not_Implemented;
   end Create_Request;

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : in     Flags  := 0) is
   begin
      PolyORB.Requests.Invoke (Self.The_Request);
      --  XXX Some arguments are not taken into account!
   end Invoke;

   procedure Delete (Self : in out Object) is
   begin
      PolyORB.Requests.Destroy_Request (Self.The_Request);
   end Delete;

   function To_PolyORB_Request
     (Request : Object)
     return PolyORB.Requests.Request_Access is
   begin
      return Request.The_Request;
   end To_PolyORB_Request;

   procedure Finalize (X : in out Object) is
   begin
      PolyORB.Requests.Destroy_Request (X.The_Request);
   end Finalize;

end CORBA.Request;

