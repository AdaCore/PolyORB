------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        C O R B A . R E Q U E S T                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  The CORBA Dynamic Invocation Interface.

with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.Interceptors_Hooks;

with PolyORB.Errors.Helper;
with PolyORB.Initialization;
with PolyORB.QoS.Addressing_Modes;
with PolyORB.References;
with PolyORB.Request_QoS;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

with CORBA.Object;

package body CORBA.Request is

   procedure Default_Invoke
     (Request : access PolyORB.Requests.Request;
      Flags   : PolyORB.Requests.Flags);
   --  Default request invocation subprogram

   --------------------
   -- Create_Request --
   --------------------

   procedure Create_Request
     (Self      : CORBA.AbstractBase.Ref;
      Ctx       : CORBA.Context.Ref;
      Operation : Identifier;
      Arg_List  : CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   : out Object;
      Req_Flags : Flags)
   is
      pragma Unreferenced (Ctx);
      pragma Unreferenced (Req_Flags);

      PResult : PolyORB.Any.NamedValue :=
                  (Name      => PolyORB.Types.Identifier (Result.Name),
                   Argument  => PolyORB.Any.Any (Result.Argument),
                   Arg_Modes => PolyORB.Any.Flags (Result.Arg_Modes));

   begin
      PolyORB.Requests.Setup_Request
        (Req       => Request.The_Request,
         Target    => CORBA.Object.Internals.To_PolyORB_Ref
                        (CORBA.Object.Ref (AbstractBase.Ref'Class (Self))),
         Operation => To_Standard_String (Operation),
         Arg_List  => NVList.Internals.To_PolyORB_Ref (Arg_List),
         Result    => PResult,
         Req_Flags => PolyORB.Requests.Default_Flags);
      --  For now, we use the default flags???
   end Create_Request;

   procedure Create_Request
     (Self      : CORBA.AbstractBase.Ref;
      Ctx       : CORBA.Context.Ref;
      Operation : Identifier;
      Arg_List  : CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Exc_List  : ExceptionList.Ref;
      Ctxt_List : ContextList.Ref;
      Request   : out CORBA.Request.Object;
      Req_Flags : Flags)
   is
      pragma Unreferenced (Ctx, Ctxt_List);
      pragma Unreferenced (Req_Flags);

      PResult : PolyORB.Any.NamedValue
        := (Name      => PolyORB.Types.Identifier (Result.Name),
            Argument  => PolyORB.Any.Any (Result.Argument),
            Arg_Modes => PolyORB.Any.Flags (Result.Arg_Modes));

   begin
      PolyORB.Requests.Setup_Request
        (Req       => Request.The_Request,
         Target    => CORBA.Object.Internals.To_PolyORB_Ref
                        (CORBA.Object.Ref (AbstractBase.Ref'Class (Self))),
         Operation => To_Standard_String (Operation),
         Arg_List  => CORBA.NVList.Internals.To_PolyORB_Ref (Arg_List),
         Result    => PResult,
         Exc_List  => CORBA.ExceptionList.Internals.To_PolyORB_Ref (Exc_List),
         Req_Flags => PolyORB.Requests.Default_Flags);
      --  For now, we use the default flags???
   end Create_Request;

   --------------------
   -- Default_Invoke --
   --------------------

   procedure Default_Invoke
     (Request : access PolyORB.Requests.Request;
      Flags   : PolyORB.Requests.Flags)
   is
      use type PolyORB.Any.TypeCode.Local_Ref;

   begin
      loop
         PolyORB.Requests.Invoke (Request, Flags);

         exit when PolyORB.Any.Is_Empty (Request.Exception_Info)
           or else (PolyORB.Any.Get_Type (Request.Exception_Info)
                      /= PolyORB.Errors.Helper.TC_ForwardRequest
             and then PolyORB.Any.Get_Type (Request.Exception_Info)
                      /= PolyORB.Errors.Helper.TC_NeedsAddressingMode);

         --  Prepare request for new target

         if PolyORB.Any.Get_Type (Request.Exception_Info)
              = PolyORB.Errors.Helper.TC_ForwardRequest
         then
            --  Location forwarding

            declare
               Members : constant PolyORB.Errors.ForwardRequest_Members
                 := PolyORB.Errors.Helper.From_Any (Request.Exception_Info);
               Ref     : PolyORB.References.Ref;

            begin
               PolyORB.References.Set
                 (Ref,
                  PolyORB.Smart_Pointers.Entity_Of
                  (Members.Forward_Reference));

               PolyORB.Requests.Reset_Request (Request.all);
               Request.Target := Ref;
            end;

         else
            --  GIOP Addressing Mode change

            declare
               use PolyORB.QoS;
               use PolyORB.QoS.Addressing_Modes;
               use PolyORB.Request_QoS;

               Members : constant PolyORB.Errors.NeedsAddressingMode_Members
                 := PolyORB.Errors.Helper.From_Any (Request.Exception_Info);

            begin
               PolyORB.Requests.Reset_Request (Request.all);

               Add_Request_QoS
                 (Request.all,
                  GIOP_Addressing_Mode,
                  new QoS_GIOP_Addressing_Mode_Parameter'
                    (Kind => GIOP_Addressing_Mode,
                     Mode => Members.Mode));
            end;
         end if;
      end loop;
   end Default_Invoke;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : Flags  := 0)
   is
   begin
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Self.The_Request'Access, PolyORB.Requests.Flags (Invoke_Flags));

      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence (Self.The_Request);
   end Invoke;

   ------------
   -- Delete --
   ------------

   procedure Delete (Self : in out Object) is
   begin
      null;
   end Delete;

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
       Init      => Initialize'Access,
       Shutdown  => null));
end CORBA.Request;
