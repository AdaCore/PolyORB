with Echo.Stream;
with CORBA;
with Broca.Marshalling;
with Broca.Giop;
with Broca.Exceptions;
with Echo;
package body Echo.Impl is

   type Object_Acc is access all Object'Class;

   function Get_Type_Id (Obj : Object) return CORBA.RepositoryId is
   begin
      return CORBA.To_CORBA_String ("IDL:Echo:1.0");
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
      if Operation = "echoString" then
         declare
            Mesg : CORBA.String;
            Returns : CORBA.String;
         begin
            --  Unmarshalls arguments
            Unmarshall (Stream, Mesg);
            --  Call implementation
            Returns := echoString (Object_Acc (Obj), Mesg);
            Stream.Pos := Broca.Giop.Message_Header_Size;
            --  service context
            Marshall_Size_Unsigned_Long (Stream);
            --  Request_id
            Marshall_Size_Unsigned_Long (Stream);
            --  reply_status
            Marshall_Size_Unsigned_Long (Stream);
            --  return argument
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
            --  return arg
            Marshall (Stream, Mesg);
            return;

         exception
            when E : no_walls =>
               declare
                  use Echo.Stream;
                  RepoID : String
                     := "IDL:Echo/no_walls:1.0";
                  Member : no_walls_Members;
               begin
                  Echo.Get_Members (E, Member);
                  --  Compute size of reply
                  Stream.Pos := Broca.Giop.Message_Header_Size;
                  --  service context
                  Marshall_Size_Unsigned_Long (Stream);
                  --  request id
                  Marshall_Size_Unsigned_Long (Stream);
                  --  reply status
                  Marshall_Size_Unsigned_Long (Stream);
                  Marshall_Size (Stream, RepoID);
                  Marshall_Size (Stream, Member);
                  Reply_Size :=
                     Stream.Pos - Broca.Giop.Message_Header_Size;
                  Increase_Buffer_And_Clear_Pos (Stream, Stream.Pos);

                  Broca.Giop.Create_Giop_Header
                     (Stream, Broca.Giop.Reply,
                      CORBA.Unsigned_Long (Reply_Size));

                  --  service context
                  Marshall (Stream, CORBA.Unsigned_Long (0));
                  --  request id
                  Marshall (Stream, Request_Id);
                  --  reply status
                  Marshall (Stream, Broca.Giop.User_Exception);

                  --  Marshall exception
                  Marshall (Stream, RepoID);
                  Marshall (Stream, Member);
                  return;
               end;
         end;
      end if;

      Broca.Exceptions.Raise_Bad_Operation;
   end Giop_Dispatch;

end Echo.Impl;
