with Ada.Text_IO; use Ada.Text_IO;
with Exceptions;  use Exceptions;

package body Common is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Penpal : in out Penpal_Type;
      Name   : in String)
   is
   begin
      if Name = "" then
         raise Sender_Error;
      end if;
      Penpal.Name := new String'(Name);
   end Initialize;

   -------------
   -- Name_Of --
   -------------

   function Name_Of (Penpal : access Penpal_Type) return String is
   begin
      if Penpal.Name = null then
         raise Sender_Error;
      else
         return Penpal.Name.all;
      end if;
   end Name_Of;

   -----------------
   -- New_Message --
   -----------------

   procedure New_Message
     (Sender    : in String;
      Recipient : access Penpal_Type;
      Message   : in String)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Recipient);
      pragma Warnings (On);
   begin
      if Sender = "" then
         raise Sender_Error;
      elsif Message = "" then
         raise Message_Error;
      else
         Put_Line ("New message: <" & Sender & "> " & Message);
      end if;
   end New_Message;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Penpal : out String_Access)
   is
   begin
      --  No need to use this

      raise Program_Error;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Penpal : in String_Access)
   is
   begin
      --  No need to use this

      raise Program_Error;
   end Write;

end Common;
