with Echo.Impl;
with Echo.Helper;

with CORBA;
with CORBA.Object;

with Broca.Basic_Startup; use Broca.Basic_Startup;
pragma Elaborate (Broca.Basic_Startup);

with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure Server is
   Ref : CORBA.Object.Ref;
   task T is
      entry Start;
   end T;

   task body T is
      MyEcho   : Echo.Ref;
      Sent_Msg : CORBA.String;
      Recv_Msg : CORBA.String;

   begin
      accept Start;
      Ada.Text_IO.Put_Line ("wait for 5 s");
      delay 5.0;
      Ada.Text_IO.Put_Line ("active client");
      MyEcho := Echo.Helper.To_Ref (Ref);

      --  checking if it worked
      if Echo.Is_Nil (myecho) then
         Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
         raise Program_Error;
      end if;

      --  sending message
      Sent_Msg := CORBA.To_CORBA_String (Standard.String'("Hello Ada !"));
      --  Sent_Msg := CORBA.To_CORBA_String (Standard.String'("Hello!"));
      Recv_Msg := Echo.echoString (myecho, Sent_Msg);

      --  printing result
      Ada.Text_IO.Put_Line
        ("I said : " & CORBA.To_Standard_String (Sent_Msg));
      Ada.Text_IO.Put_Line
        ("The object answered : " & CORBA.To_Standard_String (Recv_Msg));

   exception when E: others =>

      Ada.Text_IO.Put_Line ("exception " & Exception_Name (E));
      Ada.Text_IO.Put_Line (Exception_Message (E));
   end T;

begin
   Initiate_Servant (new Echo.Impl.Object, Ref);
   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
      "'");
   T.Start;
   Initiate_Server;
end Server;
