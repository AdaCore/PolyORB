------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        C O R B A . R E Q U E S T                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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

--  The CORBA Dynamic Invocation Interface.

--  $Id$

with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.Interceptors_Hooks;

with PolyORB.Initialization;
with PolyORB.Requests;
with PolyORB.References;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

with CORBA.Context;
with CORBA.NVList;
with CORBA.Object;

package body CORBA.Request is

   procedure Default_Invoke
     (Request : in PolyORB.Requests.Request_Access;
      Flags   : in PolyORB.Requests.Flags);
   --  Default request invocation subprogram

   --------------------
   -- Create_Request --
   --------------------

   procedure Create_Request
     (Self      : in     CORBA.AbstractBase.Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   :    out Object;
      Req_Flags : in     Flags)
   is
      pragma Unreferenced (Ctx);
      pragma Unreferenced (Req_Flags);

      PResult : PolyORB.Any.NamedValue
        := (Name      => PolyORB.Types.Identifier (Result.Name),
            Argument  => Internals.To_PolyORB_Any (Result.Argument),
            Arg_Modes => PolyORB.Any.Flags (Result.Arg_Modes));

   begin
      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (CORBA.AbstractBase.Ref'Class (Self))),
         Operation => To_Standard_String (Operation),
         Arg_List  => CORBA.NVList.To_PolyORB_Ref (Arg_List),
         Result    => PResult,
         Req       => Request.The_Request,
         Req_Flags => PolyORB.Requests.Default_Flags);
      --  XX For now, we use the default flags
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
      Req_Flags : in     Flags)
   is
      pragma Unreferenced (Ctx, Ctxt_List);
      pragma Unreferenced (Req_Flags);

      PResult : PolyORB.Any.NamedValue
        := (Name      => PolyORB.Types.Identifier (Result.Name),
            Argument  => Internals.To_PolyORB_Any (Result.Argument),
            Arg_Modes => PolyORB.Any.Flags (Result.Arg_Modes));

   begin
      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (CORBA.AbstractBase.Ref'Class (Self))),
         Operation => To_Standard_String (Operation),
         Arg_List  => CORBA.NVList.To_PolyORB_Ref (Arg_List),
         Result    => PResult,
         Exc_List  => CORBA.ExceptionList.To_PolyORB_Ref (Exc_List),
         Req       => Request.The_Request,
         Req_Flags => PolyORB.Requests.Default_Flags);
      --  XX For now, we use the default flags
   end Create_Request;

   --------------------
   -- Default_Invoke --
   --------------------

   procedure Default_Invoke
     (Request : in PolyORB.Requests.Request_Access;
      Flags   : in PolyORB.Requests.Flags)
   is
      use type PolyORB.Any.TypeCode.Object;
      use type PolyORB.Requests.Request_Access;

      Cur_Req : PolyORB.Requests.Request_Access := Request;

   begin
      loop
         PolyORB.Requests.Invoke (Cur_Req, Flags);

         exit when PolyORB.Any.Is_Empty (Cur_Req.Exception_Info)
           or else PolyORB.Any.Get_Type (Cur_Req.Exception_Info)
                     /= PolyORB.Exceptions.TC_ForwardRequest;

         --  Prepare request for new target

         declare
            Members : constant PolyORB.Exceptions.ForwardRequest_Members
              := PolyORB.Exceptions.From_Any (Cur_Req.Exception_Info);
            Ref     : PolyORB.References.Ref;
            Aux_Req : PolyORB.Requests.Request_Access;
         begin
            PolyORB.References.Set
              (Ref,
               PolyORB.Smart_Pointers.Entity_Of (Members.Forward_Reference));

            PolyORB.Requests.Create_Request
              (Target    => Ref,
               Operation => PolyORB.Types.To_String (Request.Operation),
               Arg_List  => Request.Args,
               Result    => Request.Result,
               Exc_List  => Request.Exc_List,
               Req       => Aux_Req,
               Req_Flags => Request.Req_Flags);

            if Cur_Req /= Request then
               PolyORB.Requests.Destroy_Request (Cur_Req);
            end if;

            Cur_Req := Aux_Req;
         end;
      end loop;

      if Cur_Req /= Request then
         --  Auxiliary request allocated, copy request results from it to
         --  original request and destroy auxiliary request.

         Request.Args           := Cur_Req.Args;
         Request.Out_Args       := Cur_Req.Out_Args;
         Request.Result         := Cur_Req.Result;
         Request.Exception_Info := Cur_Req.Exception_Info;

         PolyORB.Requests.Destroy_Request (Cur_Req);
      end if;
   end Default_Invoke;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : in     Flags  := 0)
   is
   begin
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Self.The_Request, PolyORB.Requests.Flags (Invoke_Flags));

      if not PolyORB.Any.Is_Empty (Self.The_Request.Exception_Info) then
         --  XXX warning, should verify that the raised exception
         --  is either a system exception or a declared user
         --  exception before propagating it: if an unknown
         --  user exception gets up to here, CORBA.UNKNOWN
         --  must be raised.

         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Self.The_Request.Exception_Info);
      end if;
   end Invoke;

   ------------
   -- Delete --
   ------------

   procedure Delete (Self : in out Object) is
   begin
      PolyORB.Requests.Destroy_Request (Self.The_Request);
   end Delete;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Object) is
   begin
      PolyORB.Requests.Destroy_Request (X.The_Request);
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        := Default_Invoke'Access;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"corba.request",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end CORBA.Request;
