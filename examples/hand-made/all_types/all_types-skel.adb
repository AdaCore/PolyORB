with all_types.Stream;
with CORBA;
with Broca.CDR;
--  with Broca.Refs;
with Broca.GIOP;
with Broca.Exceptions;
with all_types;
use all_types.Stream;
use CORBA;
use Broca.CDR;
--  use Broca.Refs;
use Broca.GIOP;
use Broca.Exceptions;
use all_types;
package body all_types.Skel is

   type Object_Ptr is access all Object'Class;

   function Get_Type_Id (Obj : Object) return CORBA.RepositoryId is
   begin
      return CORBA.To_CORBA_String ("IDL:all_types:1.0");
   end Get_Type_Id;

   procedure GIOP_Dispatch
     (Obj : access Object;
      Operation : String;
      Request_Id : CORBA.Unsigned_Long;
      Response_Expected : CORBA.Boolean;
      Request_Buffer : access Broca.Buffers.Buffer_Type;
      Reply_Buffer   : access Broca.Buffers.Buffer_Type)
   is
      use Broca.CDR;
      --  use Broca.Refs;
      use Broca.Buffers;
   begin
      if Operation = "echoBoolean" then
         declare
            IDL_arg : CORBA.Boolean;
            Returns : CORBA.Boolean;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoBoolean (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer,
                                 Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoShort" then
         declare
            IDL_arg : CORBA.Short;
            Returns : CORBA.Short;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoShort (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoLong" then
         declare
            IDL_arg : CORBA.Long;
            Returns : CORBA.Long;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoLong (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoUShort" then
         declare
            IDL_arg : CORBA.Unsigned_Short;
            Returns : CORBA.Unsigned_Short;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoUShort (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoULong" then
         declare
            IDL_arg : CORBA.Unsigned_Long;
            Returns : CORBA.Unsigned_Long;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoULong (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoFloat" then
         declare
            IDL_arg : CORBA.Float;
            Returns : CORBA.Float;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoFloat (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoDouble" then
         declare
            IDL_arg : CORBA.Double;
            Returns : CORBA.Double;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoDouble (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoChar" then
         declare
            IDL_arg : CORBA.Char;
            Returns : CORBA.Char;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoChar (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoOctet" then
         declare
            IDL_arg : CORBA.Octet;
            Returns : CORBA.Octet;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoOctet (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoString" then
         declare
            IDL_arg : CORBA.String;
            Returns : CORBA.String;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoString (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoRef" then
         declare
            IDL_arg : Ref;
            Returns : Ref;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoRef (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoColor" then
         declare
            IDL_arg : Color;
            Returns : Color;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoColor (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "testException" then
         declare
            IDL_arg : CORBA.Long;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            testException (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            return;

         exception
            when E : my_exception =>
               declare
                  use all_types.Stream;
                  RepoID : CORBA.String
                    := CORBA.To_CORBA_String
                    ("IDL:all_types/my_exception:1.0");
                  Member : my_exception_Members;
               begin
                  all_types.Get_Members (E, Member);

                  --  service context
                  Marshall (Reply_Buffer,
                            CORBA.Unsigned_Long (Broca.GIOP.No_Context));
                  --  request id
                  Marshall (Reply_Buffer, Request_Id);
                  --  reply status
                  Marshall (Reply_Buffer, Broca.GIOP.User_Exception);

                  --  Marshall exception
                  Marshall (Reply_Buffer, RepoID);
                  Marshall (Reply_Buffer, Member);
                  return;
               end;
         end;
      end if;

      if Operation = "echoUnion" then
         declare
            IDL_arg : myUnion;
            Returns : myUnion;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoUnion (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoArray" then
         declare
            IDL_arg : simple_array;
            Returns : simple_array;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoArray (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoMatrix" then
         declare
            IDL_arg : matrix;
            Returns : matrix;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoMatrix (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoStruct" then
         declare
            IDL_arg : simple_struct;
            Returns : simple_struct;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoStruct (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "echoUsequence" then
         declare
            IDL_arg : U_sequence;
            Returns : U_sequence;
         begin
            --  Unmarshalls arguments
            IDL_arg := Unmarshall (Request_Buffer);
            --  Call implementation
            Returns := echoUsequence (Object_Ptr (Obj), IDL_arg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "_get_Counter" then
         declare
            Returns : CORBA.Long;
         begin
            --  Call implementation
            Returns := Get_Counter (Object_Ptr (Obj));

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      if Operation = "_set_myColor" then
         declare
            myColor : Color;
         begin
            --  Unmarshalls arguments
            myColor := Unmarshall (Request_Buffer);
            --  Call implementation
            Set_myColor (Object_Ptr (Obj), myColor);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            return;
         end;
      end if;

      if Operation = "_get_myColor" then
         declare
            Returns : Color;
         begin
            --  Call implementation
            Returns := Get_myColor (Object_Ptr (Obj));

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer, Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer, Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);
            return;
         end;
      end if;

      Broca.Exceptions.Raise_Bad_Operation;
   end GIOP_Dispatch;

end all_types.Skel;
