------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         C O R B A . O B J E C T                          --
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

with Broca.IOR;
with Broca.Buffers; use Broca.Buffers;
with Broca.CDR;     use Broca.CDR;

with Broca.Repository;
with Broca.GIOP;
with Broca.Exceptions;
with Broca.Object;

package body CORBA.Object is

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self : Ref;
      Type_Id : CORBA.RepositoryId)
      return CORBA.Boolean
   is
      use CORBA;
      use Broca.Repository;

   begin
      --  Any object Is_A CORBA::Object.

      if Is_Equivalent
        (Type_Id,
         CORBA.To_CORBA_String
         ("IDL:omg.org/CORBA/Object:1.0"))
      then
         return True;
      end if;

      --  Any object is of the class of its
      --  actual type.

      if Is_Equivalent
        (CORBA.RepositoryId
         (Broca.Object.Object_Ptr
          (Object_Of (Self)).Type_Id),
         Type_Id)
      then
         return True;
      end if;

      --  If class membership cannot be determined locally,
      --  perform a remote call on the object.

      --  Some of this code is replicated from the generated
      --  stubs code for object operations.

      declare
         Returns : CORBA.Boolean;
         is_a_Operation : constant CORBA.Identifier
           := CORBA.To_CORBA_String ("_is_a");
         Handler : Broca.GIOP.Request_Handler;
         Send_Request_Result : Broca.GIOP.Send_Request_Result_Type;
      begin
         if Is_Nil (Self) then
            Broca.Exceptions.Raise_Inv_Objref;
         end if;

         loop
            Broca.GIOP.Send_Request_Marshall
              (Handler, Broca.Object.Object_Ptr
               (Object_Of (Self)), True, is_a_Operation);
            Marshall
              (Handler.Buffer'Access,
               Type_Id);

            Broca.GIOP.Send_Request_Send
              (Handler, Broca.Object.Object_Ptr
               (Object_Of (Self)), True, Send_Request_Result);
            case Send_Request_Result is
               when Broca.GIOP.Sr_No_Reply =>
                  Broca.GIOP.Release (Handler);
                  Broca.GIOP.Release (Handler);
                  raise Program_Error;
               when Broca.GIOP.Sr_Forward =>
                  null;
               when Broca.GIOP.Sr_Reply =>

                  --  Unmarshall return value.
                  Returns := Unmarshall (Handler.Buffer'Access);

                  Broca.GIOP.Release (Handler);
                  return Returns;
               when Broca.GIOP.Sr_User_Exception =>
                  Broca.GIOP.Release (Handler);
                  raise Program_Error;
            end case;
         end loop;
      end;
   end Is_A;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Obj : CORBA.Object.Ref)
     return CORBA.String
   is
      Ref_Buffer : aliased Buffer_Type;
      IOR_Buffer : aliased Buffer_Type;
   begin
      Start_Encapsulation (Ref_Buffer'Access);

      Broca.CDR.Marshall (Ref_Buffer'Access, Obj);
      Marshall (IOR_Buffer'Access, Encapsulate (Ref_Buffer'Access));

      declare
         Result : constant CORBA.String
           := Broca.IOR.Buffer_To_IOR_String (IOR_Buffer'Access);
      begin
         Release (IOR_Buffer);
         Release (Ref_Buffer);
         return Result;
      end;
   end Object_To_String;

   --------------------
   -- Create_Request --
   --------------------

   procedure Create_Request
     (Self      : in     Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags) is
   begin
      CORBA.Request.Create_Request (CORBA.AbstractBase.Ref (Self),
                                    Ctx,
                                    Operation,
                                    Arg_List,
                                    Result,
                                    Request,
                                    Req_Flags);
   end Create_Request;

end CORBA.Object;
