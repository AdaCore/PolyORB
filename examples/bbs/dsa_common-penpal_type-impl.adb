----------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://www.polyorb.eu.org/)
----------------------------------------------

with Ada.Text_IO;

with CORBA;
with DSA_Common.Penpal_Type.Skel;
pragma Elaborate (DSA_Common.Penpal_Type.Skel);
pragma Warnings (Off, DSA_Common.Penpal_Type.Skel);

package body DSA_Common.Penpal_Type.Impl is


   procedure Initialize
     (Self : access Object;
      Name : in CORBA.String) is
   begin
      Self.Name := Name;
   end Initialize;


   function Name_Of
     (Self : access Object)
     return CORBA.String
   is
   begin
      return Self.Name;
   end Name_Of;


   procedure New_Message
     (Self : access Object;
      Sender : in CORBA.String;
      Message : in CORBA.String)
   is
      pragma Unreferenced (Self);
      use CORBA;

      A_Sender : constant String := To_Standard_String (Sender);
      A_Message : constant String := To_Standard_String (Message);
   begin
--        if A_Sender = "" then
--           raise Sender_Error;
--        elsif A_Message = "" then
--           raise Message_Error;
--        else
         Ada.Text_IO.Put_Line
           ("New message: <" & A_Sender & "> " & A_Message);
--        end if;
   end New_Message;

end DSA_Common.Penpal_Type.Impl;
