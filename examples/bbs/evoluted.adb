with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Real_Time;    use Ada.Real_Time;
with Exceptions;       use Exceptions;
with Evoluted_Pkg;     use Evoluted_Pkg;
with Server;           use Server;
with Common;
with Utils;

with PolyORB.Dynamic_Dict;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;

procedure Evoluted is

   --  This program is launched using: evoluted "pseudo"

   procedure Usage;
   --  Print usage

   package Penpals_Cache is new PolyORB.Dynamic_Dict
     (Penpal_Pointer, null);

   function Cache_Get_Penpal (P : String) return Penpal_Pointer;
   function Cache_Get_Penpal (P : String) return Penpal_Pointer is
      PP : Penpal_Pointer := Penpals_Cache.Lookup (P, null);
   begin
      if PP = null then
         PP := Get_Penpal (P);
         Penpals_Cache.Register (P, PP);
      end if;
      return PP;
   end Cache_Get_Penpal;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line ("Usage: evoluted ""nickname""");
      Set_Exit_Status (1);
   end Usage;

   Is_Test : Boolean := False;
   Message_Count : Integer;
   Message_Size : Integer;
   Nmax : Integer;
   type String_Ptr is access String;
   Payload : String_Ptr;
   Start : Time;

   function Image (I : Integer) return String;
   function Image (I : Integer) return String
   is
      S : constant String := Integer'Image (I);
      First : Integer := S'First;
   begin
      if S (First) = ' ' then
         First := First + 1;
      end if;
      return S (First .. S'Last);
   end Image;

begin
   loop
      case Getopt ("c: n: s:") is
         when ASCII.NUL => exit;

         when 'c' =>
            Message_Count := Integer'Value (Parameter);

         when 'n' =>
            Nmax := Integer'Value (Parameter);
            Is_Test := True;

         when 's' =>
            Message_Size := Integer'Value (Parameter);

         when others =>
            raise Program_Error;
      end case;
   end loop;

   declare
      Penpal_Name : constant String := Get_Argument;
   begin
      if Penpal_Name'Length = 0 then
         Usage;
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Put ("Initializing local penpal...");
      Initialize (Penpal, Penpal_Name);
      Put (" registering...");
      Register (Penpal'Access);
      Put_Line (" done.");

      if not Is_Test then
         Mainloop;
         GNAT.OS_Lib.OS_Exit (0);
      end if;

      ----------------------------
      -- Automated test section --
      ----------------------------

      Expected_Messages := 2 * Nmax * Message_Count;
      Payload := new String'(1 .. Message_Size => 'X');

      Put_Line ("Expecting"
                  & Expected_Messages'Img & " messages.");
      declare
         Dummy : constant String := Utils.Get_Line ("Ready>");
         pragma Unreferenced (Dummy);
      begin
         null;
      end;

      Start := Clock;
      for K in 0 .. Message_Count - 1 loop
         declare
            Iter : constant String := Integer'Image (K);
         begin
            Post_Message
              (Sender  => Name_Of (Penpal'Access),
               Message => "B" & Iter & ":" & Penpal_Name
                 & ":" & Payload.all);

            for J in 0 .. Nmax - 1 loop
               declare
                  To : constant String := "TEST_" & Image (J);
               begin
                  Common.New_Message
                    (Sender    => Name_Of (Penpal'Access),
                     Recipient => Cache_Get_Penpal (To),
                     Message   => "P" & Iter & ":"
                       & Penpal_Name & ":" & To
                       & ":" & Payload.all);
               end;
            end loop;
         exception
            when E : others =>
               Put_Line ("Raised exception in iteration"
                 & Integer'Image (K) & ": "
                 & Ada.Exceptions.Exception_Information (E));
               GNAT.OS_Lib.OS_Exit (1);
         end;
      end loop;

      Send_Done := True;
      Ada.Text_IO.Put_Line ("Send done.");

      while not Recv_Done loop
         delay 0.1;
      end loop;
      declare
         Elapsed : constant Duration
           := To_Duration (Clock - Start);
      begin
         Put_Line ("Elapsed :" & Duration'Image (Elapsed));
      end;
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end;
   delay 5.0;
   GNAT.OS_Lib.OS_Exit (0);

exception
   when Sender_Error =>
      Put_Line ("Invalid nickname");
      Set_Exit_Status (2);
end Evoluted;
