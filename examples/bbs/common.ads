with Ada.Streams;

package Common is

   pragma Remote_Types;

   type Penpal_Type is tagged limited private;
   --  One particular person

   procedure Initialize
     (Penpal : in out Penpal_Type;
      Name   : in String);
   --  Initialize a Penpal name. This will raise Sender_Error if the
   --  Name is empty. You must register this penpal to get new incoming
   --  messages.

   function Name_Of (Penpal : access Penpal_Type) return String;
   --  Return the name of a Penpal, or raise Sender_Error if the name
   --  has not been set.

   procedure New_Message
     (Sender    : in String;
      Recipient : access Penpal_Type;
      Message   : in String);
   --  This procedure will be called when the penpal has registered itself
   --  and a new message arrives on the BBS. Sender_Error or Message_Error
   --  will be raised if Sender or Message are empty.

private

   type String_Access is access String;

   type Penpal_Type is tagged limited record
      Name : String_Access;
   end record;

   --  Legality stuff that allows a penpal name to be transferred over the
   --  network if needed.

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Penpal : out String_Access);
   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Penpal : in String_Access);
   for String_Access'Read use Read;
   for String_Access'Write use Write;

end Common;
