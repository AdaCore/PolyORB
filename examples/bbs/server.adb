with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;
with Exceptions;              use Exceptions;
with System.RPC;
with Utils;                   use Utils;

package body Server is

   type String_Access is access String;

   type Message;
   type Message_Access is access Message;
   type Message is record
      Sender       : String_Access;
      Content      : String_Access;
      Next_Message : Message_Access;
   end record;

   protected Message_Board is

      procedure Add_Message
        (Sender  : in String;
         Content : in String);
      --  Add a message to the message board. Sender_Error and Message_Error
      --  will be raised if the sender or the message are empty.

      function Messages_Count return Natural;
      --  Number of messages in the message board

      function Get_Sender (N : Positive) return String;
      --  Sender of a particular message (No_Such_Message will be raised if
      --  there is no such message).

      function Get_Message (N : Positive) return String;
      --  Content of a particular message (No_Such_Message will be raised if
      --  there is no such message).

   private

      function Get (N : Positive) return Message_Access;
      --  Get a message, and raise No_Such_Message if it does not exist

      Messages : Message_Access := null;
      Count    : Natural        := 0;

   end Message_Board;
   --  Message_Board is a protected structure (we will not have concurrent
   --  calls) in which messages are stored. The messages are stored in reverse
   --  order. This is totally *inefficient*, but well, this is a toy program :)

   type Penpal_Node;
   type Penpal_List is access Penpal_Node;
   type Penpal_Node is record
      Name        : String_Access;
      Penpal      : Penpal_Pointer;
      Next_Penpal : Penpal_List;
   end record;

   type Penpal_Array is array (Positive range <>) of Penpal_Node;
   --  List of penpals with their names. The Next_Penpal field will have
   --  no meaning though.

   protected Penpals_Handler is

      procedure Add (Penpal : in Penpal_Pointer);
      --  Add a Penpal to the list, raise Sender_Error if the penpal has not
      --  been initialized.

      function Lookup (Name : String) return Penpal_Pointer;
      --  Lookup a penpal in the list, or raise No_Such_Penpal if no penpal
      --  by this name has been registered.

      function Get_List return Penpal_Array;
      --  Return the list of registered penpals

   private

      function Lookup (Name : String) return Penpal_List;
      --  Lookup a penpal by its name, and return null if no such penpal
      --  has been registered;

      Penpals : Penpal_List := null;

      Count   : Natural := 0;

   end Penpals_Handler;

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast (Sender : in String; Message : in String) is
      Penpals : constant Penpal_Array := Penpals_Handler.Get_List;
   begin
      Put_Line ("Sending a broadcast to " &
                Integer_To_String (Penpals'Length) &
                " registered clients:");
      for I in Penpals'Range loop
         begin
            Put ("   Trying to contact <" & Penpals (I) .Name.all & ">... ");
            Flush;
            New_Message (Sender    => Sender,
                         Recipient => Penpals (I).Penpal,
                         Message   => Message);
            Put_Line ("OK");
         exception
            when System.RPC.Communication_Error =>

               --  This penpal is probably dead, ignore the error...

               Put_Line ("fail");
         end;
      end loop;
   end Broadcast;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message (N : Positive) return String is
   begin
      return Message_Board.Get_Message (N);
   end Get_Message;

   ----------------
   -- Get_Penpal --
   ----------------

   function Get_Penpal (Name : String) return Penpal_Pointer is
   begin
      return Penpals_Handler.Lookup (Name);
   end Get_Penpal;

   ----------------
   -- Get_Sender --
   ----------------

   function Get_Sender (N : Positive) return String is
   begin
      return Message_Board.Get_Sender (N);
   end Get_Sender;

   -------------------
   -- Message_Board --
   -------------------

   protected body Message_Board is

      -----------------
      -- Add_Message --
      -----------------

      procedure Add_Message
        (Sender  : in String;
         Content : in String)
      is
      begin
         if Sender = "" then
            raise Sender_Error;
         elsif Content = "" then
            raise Message_Error;
         else
            Messages := new Message'(Sender       => new String'(Sender),
                                     Content      => new String'(Content),
                                     Next_Message => Messages);
            Count := Count + 1;
         end if;
      end Add_Message;

      ---------
      -- Get --
      ---------

      function Get (N : Positive) return Message_Access is
         Current : Message_Access := Messages;
      begin
         if N > Count then
            raise No_Such_Message;
         end if;
         for I in N + 1 .. Count loop
            Current := Current.Next_Message;
         end loop;
         return Current;
      end Get;

      -----------------
      -- Get_Message --
      -----------------

      function Get_Message (N : Positive) return String is
      begin
         return Get (N) .Content.all;
      end Get_Message;

      ----------------
      -- Get_Sender --
      ----------------

      function Get_Sender (N : Positive) return String is
      begin
         return Get (N) .Sender.all;
      end Get_Sender;

      --------------------
      -- Messages_Count --
      --------------------

      function Messages_Count return Natural is
      begin
         return Count;
      end Messages_Count;

   end Message_Board;

   ------------------------
   -- Number_Of_Messages --
   ------------------------

   function Number_Of_Messages return Natural is
   begin
      return Message_Board.Messages_Count;
   end Number_Of_Messages;

   ---------------------
   -- Penpals_Handler --
   ---------------------

   protected body Penpals_Handler is

      ---------
      -- Add --
      ---------

      procedure Add (Penpal : in Penpal_Pointer) is
         Name    : constant String := Name_Of (Penpal);
         Current : Penpal_List     := Lookup (Name);
      begin
         if Current = null then
            Penpals := new Penpal_Node'(Name        => new String'(Name),
                                        Penpal      => Penpal,
                                        Next_Penpal => Penpals);
            Count := Count + 1;
         else
            Current.Penpal := Penpal;
         end if;
      end Add;

      --------------
      -- Get_List --
      --------------

      function Get_List return Penpal_Array is
         Result  : Penpal_Array (1 .. Count);
         Current : Penpal_List := Penpals;
      begin
         for I in 1 .. Count loop
            Result (I) := Current.all;

            --  Since the Next_Penpal has no meaning here, clear it
            Result (I) .Next_Penpal := null;

            Current := Current.Next_Penpal;
         end loop;
         return Result;
      end Get_List;

      ------------
      -- Lookup --
      ------------

      function Lookup (Name : String) return Penpal_Pointer is
         Current : constant Penpal_List := Lookup (Name);
      begin
         if Current = null then
            raise No_Such_Penpal;
         else
            return Current.Penpal;
         end if;
      end Lookup;

      ------------
      -- Lookup --
      ------------

      function Lookup (Name : String) return Penpal_List is
         Current  : Penpal_List     := Penpals;
         Low_Name : constant String := To_Lower (Name);
      begin
         while Current /= null loop
            begin
               if To_Lower (Current.Name.all) = Low_Name then
                  return Current;
               end if;
            exception
               when System.RPC.Communication_Error =>
                  --  The given name is unreachable, don't give it, return
                  --  null instead.

                  return null;
            end;
            Current := Current.Next_Penpal;
         end loop;
         return null;
      end Lookup;

   end Penpals_Handler;

   ------------------
   -- Post_Message --
   ------------------

   procedure Post_Message
     (Sender  : in String;
      Message : in String)
   is
   begin
      --  Add the message to the message board

      Put_Line ("Posting a message from <" & Sender & ">: """ &
                Message & """");
      Message_Board.Add_Message (Sender, Message);

      --  For each registered client, send it the message

      Broadcast (Sender, Message);
   end Post_Message;

   --------------
   -- Register --
   --------------

   procedure Register (Penpal : in Penpal_Pointer) is
   begin
      Put_Line ("Registering a new penpal <" & Name_Of (Penpal) & ">");
      Penpals_Handler.Add (Penpal);
   end Register;

end Server;
