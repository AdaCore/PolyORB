with all_types.Stream;
with CORBA;
with Broca.Marshalling;
with Broca.Giop;
with Broca.Exceptions;
package body all_types.Impl is

   type Object_Acc is access all Object'Class;

   function Get_Type_Id (Obj : Object) return CORBA.RepositoryId is
   begin
      return CORBA.To_CORBA_String ("IDL:all_types:1.0");
   end Get_Type_Id;

   procedure Giop_Dispatch
     (Obj : access Object;
      Operation : String;
      Request_Id : CORBA.Unsigned_Long;
      Reponse_Expected : CORBA.Boolean;
      Stream : in out Broca.Types.Buffer_Descriptor)
   is
      use Broca.Marshalling;
      use Broca.Types;
      Reply_Size : Broca.Types.Buffer_Index_Type;
   begin
      if Operation = "echoBoolean" then
         declare
            arg : CORBA.Boolean;
            Returns : CORBA.Boolean;
         begin
            --  Unmarshalls arguments
            Unmarshall (Stream, arg);
            --  Call implementation
            Returns := echoBoolean (Object_Acc (Obj), arg);
            Stream.Pos := Broca.Giop.Message_Header_Size;
            --  service context
            Marshall_Size_Unsigned_Long (Stream);
            --  Request_id
            Marshall_Size_Unsigned_Long (Stream);
            --  reply_status
            Marshall_Size_Unsigned_Long (Stream);
            --  return value
            Marshall_Size (Stream, Returns);
            Reply_Size := Stream.Pos - Broca.Giop.Message_Header_Size;
            Increase_Buffer_And_Clear_Pos (Stream, Stream.Pos);

            Broca.Giop.Create_Giop_Header
              (Stream, Broca.Giop.Reply,
               CORBA.Unsigned_Long (Reply_Size));

            --  service context
            Marshall (Stream, CORBA.Unsigned_Long (0));
            --  request id
            Marshall (Stream, Request_Id);
            --  reply status
            Marshall (Stream, Broca.Giop.No_Exception);
            --  return value
            Marshall (Stream, Returns);
            return;
         end;
      end if;

      if Operation = "echoShort" then
         declare
            arg : CORBA.Short;
            Returns : CORBA.Short;
         begin
            --  Unmarshalls arguments
            Unmarshall (Stream, arg);
            --  Call implementation
            Returns := echoShort (Object_Acc (Obj), arg);
            Stream.Pos := Broca.Giop.Message_Header_Size;
            --  service context
            Marshall_Size_Unsigned_Long (Stream);
            --  Request_id
            Marshall_Size_Unsigned_Long (Stream);
            --  reply_status
            Marshall_Size_Unsigned_Long (Stream);
            --  return value
            Marshall_Size (Stream, Returns);
            Reply_Size := Stream.Pos - Broca.Giop.Message_Header_Size;
            Increase_Buffer_And_Clear_Pos (Stream, Stream.Pos);

            Broca.Giop.Create_Giop_Header
              (Stream, Broca.Giop.Reply,
               CORBA.Unsigned_Long (Reply_Size));

            --  service context
            Marshall (Stream, CORBA.Unsigned_Long (0));
            --  request id
            Marshall (Stream, Request_Id);
            --  reply status
            Marshall (Stream, Broca.Giop.No_Exception);
            --  return value
            Marshall (Stream, Returns);
            return;
         end;
      end if;

      if Operation = "echoLong" then
         declare
            arg : CORBA.Long;
            Returns : CORBA.Long;
         begin
            --  Unmarshalls arguments
            Unmarshall (Stream, arg);
            --  Call implementation
            Returns := echoLong (Object_Acc (Obj), arg);
            Stream.Pos := Broca.Giop.Message_Header_Size;
            --  service context
            Marshall_Size_Unsigned_Long (Stream);
            --  Request_id
            Marshall_Size_Unsigned_Long (Stream);
            --  reply_status
            Marshall_Size_Unsigned_Long (Stream);
            --  return value
            Marshall_Size (Stream, Returns);
            Reply_Size := Stream.Pos - Broca.Giop.Message_Header_Size;
            Increase_Buffer_And_Clear_Pos (Stream, Stream.Pos);

            Broca.Giop.Create_Giop_Header
              (Stream, Broca.Giop.Reply,
               CORBA.Unsigned_Long (Reply_Size));

            --  service context
            Marshall (Stream, CORBA.Unsigned_Long (0));
            --  request id
            Marshall (Stream, Request_Id);
            --  reply status
            Marshall (Stream, Broca.Giop.No_Exception);
            --  return value
            Marshall (Stream, Returns);
            return;
         end;
      end if;

      if Operation = "echoUShort" then
         declare
            arg : CORBA.Unsigned_Short;
            Returns : CORBA.Unsigned_Short;
         begin
            --  Unmarshalls arguments
            Unmarshall (Stream, arg);
            --  Call implementation
            Returns := echoUShort (Object_Acc (Obj), arg);
            Stream.Pos := Broca.Giop.Message_Header_Size;
            --  service context
            Marshall_Size_Unsigned_Long (Stream);
            --  Request_id
            Marshall_Size_Unsigned_Long (Stream);
            --  reply_status
            Marshall_Size_Unsigned_Long (Stream);
            --  return value
            Marshall_Size (Stream, Returns);
            Reply_Size := Stream.Pos - Broca.Giop.Message_Header_Size;
            Increase_Buffer_And_Clear_Pos (Stream, Stream.Pos);

            Broca.Giop.Create_Giop_Header
              (Stream, Broca.Giop.Reply,
               CORBA.Unsigned_Long (Reply_Size));

            --  service context
            Marshall (Stream, CORBA.Unsigned_Long (0));
            --  request id
            Marshall (Stream, Request_Id);
            --  reply status
            Marshall (Stream, Broca.Giop.No_Exception);
            --  return value
            Marshall (Stream, Returns);
            return;
         end;
      end if;

      if Operation = "echoULong" then
         declare
            arg : CORBA.Unsigned_Long;
            Returns : CORBA.Unsigned_Long;
         begin
            --  Unmarshalls arguments
            Unmarshall (Stream, arg);
            --  Call implementation
            Returns := echoULong (Object_Acc (Obj), arg);
            Stream.Pos := Broca.Giop.Message_Header_Size;
            --  service context
            Marshall_Size_Unsigned_Long (Stream);
            --  Request_id
            Marshall_Size_Unsigned_Long (Stream);
            --  reply_status
            Marshall_Size_Unsigned_Long (Stream);
            --  return value
            Marshall_Size (Stream, Returns);
            Reply_Size := Stream.Pos - Broca.Giop.Message_Header_Size;
            Increase_Buffer_And_Clear_Pos (Stream, Stream.Pos);

            Broca.Giop.Create_Giop_Header
              (Stream, Broca.Giop.Reply,
               CORBA.Unsigned_Long (Reply_Size));

            --  service context
            Marshall (Stream, CORBA.Unsigned_Long (0));
            --  request id
            Marshall (Stream, Request_Id);
            --  reply status
            Marshall (Stream, Broca.Giop.No_Exception);
            --  return value
            Marshall (Stream, Returns);
            return;
         end;
      end if;

      if Operation = "echoChar" then
         declare
            arg : CORBA.Char;
            Returns : CORBA.Char;
         begin
            --  Unmarshalls arguments
            Unmarshall (Stream, arg);
            --  Call implementation
            Returns := echoChar (Object_Acc (Obj), arg);
            Stream.Pos := Broca.Giop.Message_Header_Size;
            --  service context
            Marshall_Size_Unsigned_Long (Stream);
            --  Request_id
            Marshall_Size_Unsigned_Long (Stream);
            --  reply_status
            Marshall_Size_Unsigned_Long (Stream);
            --  return value
            Marshall_Size (Stream, Returns);
            Reply_Size := Stream.Pos - Broca.Giop.Message_Header_Size;
            Increase_Buffer_And_Clear_Pos (Stream, Stream.Pos);

            Broca.Giop.Create_Giop_Header
              (Stream, Broca.Giop.Reply,
               CORBA.Unsigned_Long (Reply_Size));

            --  service context
            Marshall (Stream, CORBA.Unsigned_Long (0));
            --  request id
            Marshall (Stream, Request_Id);
            --  reply status
            Marshall (Stream, Broca.Giop.No_Exception);
            --  return value
            Marshall (Stream, Returns);
            return;
         end;
      end if;

      if Operation = "echoOctet" then
         declare
            arg : CORBA.Octet;
            Returns : CORBA.Octet;
         begin
            --  Unmarshalls arguments
            Unmarshall (Stream, arg);
            --  Call implementation
            Returns := echoOctet (Object_Acc (Obj), arg);
            Stream.Pos := Broca.Giop.Message_Header_Size;
            --  service context
            Marshall_Size_Unsigned_Long (Stream);
            --  Request_id
            Marshall_Size_Unsigned_Long (Stream);
            --  reply_status
            Marshall_Size_Unsigned_Long (Stream);
            --  return value
            Marshall_Size (Stream, Returns);
            Reply_Size := Stream.Pos - Broca.Giop.Message_Header_Size;
            Increase_Buffer_And_Clear_Pos (Stream, Stream.Pos);

            Broca.Giop.Create_Giop_Header
              (Stream, Broca.Giop.Reply,
               CORBA.Unsigned_Long (Reply_Size));

            --  service context
            Marshall (Stream, CORBA.Unsigned_Long (0));
            --  request id
            Marshall (Stream, Request_Id);
            --  reply status
            Marshall (Stream, Broca.Giop.No_Exception);
            --  return value
            Marshall (Stream, Returns);
            return;
         end;
      end if;

      if Operation = "echoString" then
         declare
            arg : CORBA.String;
            Returns : CORBA.String;
         begin
            --  Unmarshalls arguments
            Unmarshall (Stream, arg);
            --  Call implementation
            Returns := echoString (Object_Acc (Obj), arg);
            Stream.Pos := Broca.Giop.Message_Header_Size;
            --  service context
            Marshall_Size_Unsigned_Long (Stream);
            --  Request_id
            Marshall_Size_Unsigned_Long (Stream);
            --  reply_status
            Marshall_Size_Unsigned_Long (Stream);
            --  return value
            Marshall_Size (Stream, Returns);
            Reply_Size := Stream.Pos - Broca.Giop.Message_Header_Size;
            Increase_Buffer_And_Clear_Pos (Stream, Stream.Pos);

            Broca.Giop.Create_Giop_Header
              (Stream, Broca.Giop.Reply,
               CORBA.Unsigned_Long (Reply_Size));

            --  service context
            Marshall (Stream, CORBA.Unsigned_Long (0));
            --  request id
            Marshall (Stream, Request_Id);
            --  reply status
            Marshall (Stream, Broca.Giop.No_Exception);
            --  return value
            Marshall (Stream, Returns);
            return;
         end;
      end if;

      Broca.Exceptions.Raise_Bad_Operation;
   end Giop_Dispatch;

end all_types.Impl;
