with Ada.Streams.User;
use  Ada.Streams.User;
package body Types is

   procedure Std_Session_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Std_Session) is
   begin
      Unsigned'Read (Stream, Unsigned (Item));
   end Std_Session_Read;
   procedure Std_Session_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Std_Session) is
   begin
      Unsigned'Write (Stream, Unsigned (Item));
   end Std_Session_Write;

   procedure Std_Number_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Std_Number) is
   begin
      Unsigned'Read (Stream, Unsigned (Item));
   end Std_Number_Read;

   procedure Std_Number_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Std_Number) is
   begin
      Unsigned'Write (Stream, Unsigned (Item));
   end Std_Number_Write;

end Types;
