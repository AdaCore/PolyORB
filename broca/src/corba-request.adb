------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        C O R B A . R E Q U E S T                         --
--                                                                          --
--                                 B o d y                                  --
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

with Broca.GIOP;
with Broca.CDR;
with Broca.Exceptions;
with Broca.Debug;
with Broca.Locks; use Broca.Locks;

package body CORBA.Request is

   Flag : constant Natural
     := Broca.Debug.Is_Active ("corba.request");
   procedure O is new Broca.Debug.Output (Flag);

   procedure Add_Arg
     (Self      : in out Object;
      Arg_Type  : in     CORBA.TypeCode.Object;
      Value     : in     System.Address;
      Len       : in     Long;
      Arg_Flags : in     Flags) is
   begin
      null;
   end Add_Arg;

   procedure Add_Arg
     (Self : in out Object;
      Arg  : in     NamedValue) is
   begin
      CORBA.NVList.Add_Item (Self.Args_List, Arg);
   end Add_Arg;

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : in     Flags  := 0)
   is
      Target_Ref : CORBA.AbstractBase.Ref := Self.Target;
      Handler : Broca.GIOP.Request_Handler;
      Send_Request_Result : Broca.GIOP.Send_Request_Result_Type;
   begin
      pragma Debug (O ("Invoke: enter"));
      loop
         --  Create request handler
         Broca.GIOP.Send_Request_Marshall
           (Handler,
            Target_Ref,
            True,
            Self.Operation);

         pragma Debug (O ("Invoke: ready to marshall arguments"));
         --  Marshall in and inout arguments.
         CORBA.NVList.Marshall (Handler.Buffer'Access,
                                Self.Args_List);

         pragma Debug (O ("Invoke: arguments marshalled"));

         Broca.GIOP.Send_Request_Send
           (Handler,
            Target_Ref,
            True,
            Send_Request_Result);

         pragma Debug (O ("Invoke: request sent"));
         case Send_Request_Result is
            when Broca.GIOP.Sr_Reply =>

               pragma Debug (O ("Invoke: unmarshalling return value"));
               Broca.CDR.Unmarshall (Handler.Buffer'Access, Self.Result);

               pragma Debug
                 (O ("Invoke: unmarshalling inout and out arguments"));
               CORBA.NVList.Unmarshall (Handler.Buffer'Access,
                                        Self.Args_List);

               Broca.GIOP.Release (Handler);
               pragma Debug (O ("Invoke: end"));
               return;

            when Broca.GIOP.Sr_No_Reply =>
               Broca.GIOP.Release (Handler);
               pragma Debug (O ("Invoke: end"));
               raise Program_Error;

            when Broca.GIOP.Sr_User_Exception =>
               --  Try to find the returned exception in the exception
               --  list of the request.

               pragma Debug (O ("Invoke: Exc_List length is " &
                                CORBA.Unsigned_Long'Image
                                (CORBA.ExceptionList.Get_Count
                                 (Self.Exc_List))));

               declare
                  Exception_Repo_Id : CORBA.RepositoryId
                    := Broca.CDR.Unmarshall (Handler.Buffer'Access);
                  Index : CORBA.Unsigned_Long
                    := CORBA.ExceptionList.Search_Exception_Id
                    (Self.Exc_List, Exception_Repo_Id);
               begin
                  pragma Debug (O ("Invoke: Index = " &
                                   CORBA.Unsigned_Long'Image (Index)));
                  if Index > 0 then
                     declare
                        Member : UnknownUserException_Members;
                     begin
                        Member.IDL_Exception := CORBA.Get_Empty_Any
                          (CORBA.ExceptionList.Item (Self.Exc_List,
                                                     Index));
                        Broca.CDR.Unmarshall_To_Any
                          (Handler.Buffer'Access,
                           Member.IDL_Exception);
                        pragma Debug (O ("Invoke: end"));
                        Broca.GIOP.Release (Handler);
                        Broca.Exceptions.User_Raise_Exception
                          (UnknownUserException'Identity,
                           Member);
                     end;
                  else
                     Broca.GIOP.Release (Handler);
                     pragma Debug (O ("Invoke: end"));
                     raise Program_Error;
                  end if;
               end;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end Invoke;

   procedure Delete (Self : in out Object) is
   begin
      null;
   end Delete;

   procedure Send
     (Self         : in out Object;
      Invoke_Flags : in     Flags  := 0) is
   begin
      null;
   end Send;

   procedure Get_Response
     (Self         : in out Object;
      Invoke_Flags : in     Flags  := 0) is
   begin
      null;
   end Get_Response;

   function Poll_Response (Self : in Object) return Boolean is
   begin
      return False;
   end Poll_Response;


   procedure Create_Request
     (Self      : in     CORBA.AbstractBase.Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags) is
   begin
      Create_Request (Self,
                      Ctx,
                      Operation,
                      Arg_List,
                      Result,
                      CORBA.ExceptionList.Nil_Ref,
                      CORBA.ContextList.Nil_Ref,
                      Request,
                      Req_Flags);
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
      Argument : CORBA.Any;
      The_Value : Any_Content_Ptr_Ptr;
      The_Counter : Natural_Ptr;
   begin
      Lock_W (Result.Argument.Any_Lock);
      The_Value := Result.Argument.The_Value;
      The_Counter := Result.Argument.Ref_Counter;
      Result.Argument.Ref_Counter.all := Result.Argument.Ref_Counter.all + 1;
      Unlock_W (Result.Argument.Any_Lock);
      Argument := (Ada.Finalization.Controlled with
                   The_Value => The_Value,
                   The_Type => Result.Argument.The_Type,
                   As_Reference => True,
                   Ref_Counter => The_Counter,
                   Any_Lock => Result.Argument.Any_Lock);
      Request := (Ctx => Ctx,
                  Target => Self,
                  Operation => Operation,
                  Args_List => Arg_List,
                  Result => (Name => Result.Name,
                             Argument => Argument,
                             Arg_Modes => Result.Arg_Modes),
                  Exc_List  => Exc_List,
                  Ctxt_List => Ctxt_List,
                  Req_Flags => Req_Flags);
   end Create_Request;

end CORBA.Request;
