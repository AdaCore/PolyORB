------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        C O R B A . R E Q U E S T                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.4 $
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

with Dynamic_Proxy;
use Dynamic_Proxy;
with AdaBroker.OmniProxyCallWrapper;

with AdaBroker.Debug;
pragma Elaborate_All (Adabroker.Debug);

package body CORBA.Request is

   Flag : constant Natural
      := AdaBroker.Debug.Is_Active ("corba.request");
   procedure O is new AdaBroker.Debug.Output (Flag);

   -------------
   -- Add_Arg --
   -------------

   procedure Add_Arg
     (Self : in out Object;
      Arg  : in     NamedValue)
   is
   begin
      CORBA.NVList.Add_Item (Self.Args_List, Arg);
   end Add_Arg;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : in     Flags)
   is
      Operation : Operation_Proxy;
      Op_Type   : Operation_Type;
   begin
      --  set a proxy for the operation
      if CORBA.TypeCode.Kind (Get_Type (Self.Result.Argument)) = Tk_Void then
         Op_Type := Operation_Procedure;
      else
         Op_Type := Operation_Function;
      end if;
      Init (Operation,
            Self.Operation,
            Self.Args_List,
            Self.Result,
            Op_Type);
      pragma Debug (O ("Request.Invoke : dynamic proxy initialized"));
      --  invoke
      AdaBroker.OmniProxyCallWrapper.Invoke (Self.Target, Operation);

      case Op_Type is
         when Operation_Function =>
            --  we have a function
            Self.Result := Get_Function_Result (Operation);
         when Operation_Procedure =>
            --  we have a procedure
            Self.Args_List := Get_Procedure_Result (Operation);
      end case;

      pragma Debug (O ("Request.Invoke : dynamic invocation done"));
   end Invoke;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self : in out Object)
   is
   begin
      null;
   end Delete;

   ----------
   -- Send --
   ----------

   procedure Send
     (Self         : in out Object;
      Invoke_Flags : in     Flags)
   is
   begin
      null;
   end Send;

   ------------------
   -- Get_Response --
   ------------------

   procedure Get_Response
     (Self           : in out Object;
      Response_Flags : in     Flags)
   is
   begin
      null;
   end Get_Response;

   ----------------------------------------
   --  implementation defined functions  --
   ----------------------------------------

   -----------
   --  set  --
   -----------

   procedure Set
     (Self       :    out CORBA.Request.Object;
      OmniObj    : in     AdaBroker.OmniORB.OmniObject_Ptr;
      Operation  : in     CORBA.Identifier;
      Arg_List   : in     CORBA.NVList.Object;
      Result     : in     CORBA.NamedValue;
      Req_Flags  : in     CORBA.Flags;
      Returns    : in     Status) is
   begin
      Self := (OmniObj,
               Operation,
               Arg_List,
               Result,
               Req_Flags,
               Returns);
      pragma Debug
        (O ("a new request has been set with following return value"));
      pragma Debug
        (O ("id = " & CORBA.To_Standard_String (Result.Name)));
   end Set;

   --------------------
   --  Return_Value  --
   --------------------

   function Return_Value
     (Self : in CORBA.Request.Object)
      return CORBA.NamedValue
   is
   begin
      return Self.Result;
   end Return_Value;

   function Return_Arguments
     (Self : in CORBA.Request.Object)
      return CORBA.NVList.Object
   is
   begin
      return Self.Args_List;
   end Return_Arguments;

end CORBA.Request;
