--
--  $Id$
--

with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Streams;                     use Ada.Streams;
with Ada.Text_IO;                     use Ada.Text_IO;
with Interfaces.C;                    use Interfaces.C;
with Remote;                          use Remote;
with System.Garlic.Constants;         use System.Garlic.Constants;
with System.Garlic.Network_Utilities; use System.Garlic.Network_Utilities;
with System.Garlic.Streams;           use System.Garlic.Streams;
with System.Garlic.Thin;              use System.Garlic.Thin;
with Timing;                          use Timing;
with Utils;                           use Utils;

procedure Local is

   Buffer    : T := (others => 1);

   OneBuffer : Stream_Element_Access;
   SBuffer   : Stream_Element_Access;

   Before, After : Milliseconds;

   Iterations : constant := 5000;

   package FIO is new Float_IO (Float);
   use FIO;

   procedure Put_Summary (T : in String; R : access Float);

   procedure Sum (Head : in String; R : in Float);

   function Port_To_Use (Sock : access int) return Interfaces.C.unsigned_short;

   -----------------
   -- Port_To_Use --
   -----------------

   function Port_To_Use (Sock : access int)
      return Interfaces.C.unsigned_short
   is
      Name     : aliased Sockaddr_In;
      SName    : aliased Sockaddr_In;
      SNamelen : aliased C.int       := SName'Size / 8;
   begin
      Sock.all := C_Socket (PF_INET, SOCK_STREAM, 0);
      if C_Bind (Sock.all, Name'Address, Name'Size / 8) /= 0 then
         Raise_Exception (Program_Error'Identity,
                          "Unable to find a free port to bind to");
      end if;
      if C_Getsockname (Sock.all, SName'Address, SNamelen'Access) /= 0 then
         Raise_Exception (Program_Error'Identity,
                          "Unable to get port of the newly created socket");
      end if;
      if C_Listen (Sock.all, 1) /= 0 then
         Raise_Exception (Program_Error'Identity,
                          "Unable to create receive queue");
      end if;
      return Network_To_Port (SName.Sin_Port);
   end Port_To_Use;

   -----------------
   -- Put_Summary --
   -----------------

   procedure Put_Summary (T : in String; R : access Float) is
   begin
      Put_Line ("done");
      Put ("                   " & T & " summary:");
      R.all := Float (After - Before) / Float (Iterations);
      Put (R.all, Fore => 3, Aft => 3, Exp => 0);
      Put_Line ("ms");
   end Put_Summary;

   ---------
   -- Sum --
   ---------

   procedure Sum (Head : in String; R : in Float) is
   begin
      for I in 1 .. 50 loop
         if I <= Head'Length then
            Put (Head (I - Head'First + 1));
         else
            Put ('.');
         end if;
      end loop;
      Put (" ");
      Put (R, Fore => 3, Aft => 3, Exp => 0);
      Put_Line ("ms");
   end Sum;

   S   : aliased int;
   NS  : int;
   NA  : aliased Sockaddr_In;
   NAL : aliased int := NA'Size / 8;

   SET, RSET, Recept, MarshUnmarsh2000, MarshUnmarsh,
     ST, RST, AET, RAET, Setup, ATT, RAT, Marsh1000, Marsh,
     Unmarsh : aliased Float;

begin
   Put ("Testing communication... ");
   Flush;
   Synchronous_Empty_Test;
   Put_Line (" OK");

   Put ("Starting SET... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Synchronous_Empty_Test;
   end loop;
   After := Current;
   Put_Summary ("SET", SET'Access);

   Put ("Starting ST... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Synchronous_Test (Buffer);
   end loop;
   After := Current;
   Put_Summary ("ST", ST'Access);

   Put ("Starting AET... "); Flush;
   Before := Current;
   for I in 1 .. Iterations loop
      Asynchronous_Empty_Test;
   end loop;
   After := Current;
   Put_Summary ("AET", AET'Access);

   Put ("Starting AT... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Asynchronous_Test (Buffer);
   end loop;
   After := Current;
   Put_Summary ("AT", ATT'Access);

   Put ("Entering raw socket test mode... ");
   OneBuffer := new Stream_Element_Array (1 .. 1);
   SBuffer := new Stream_Element_Array (1 .. T'Size / 8);
   Enter_Test_Mode (Local'Partition_ID, Positive (Port_To_Use (S'Access)),
                    Iterations);
   NS := C_Accept (S, NA'Address, NAL'Access);
   Put_Line ("connected, starting raw socket tests");

   Put ("Starting raw SET... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Send (NS, OneBuffer);
      Receive (NS, OneBuffer);
   end loop;
   After := Current;
   Put_Summary ("raw SET", RSET'Access);

   Put ("Starting raw ST... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Send (NS, SBuffer);
      Receive (NS, SBuffer);
   end loop;
   After := Current;
   Put_Summary ("raw ST", RST'Access);

   Put ("Starting raw AET... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Send (NS, OneBuffer);
   end loop;
   After := Current;
   Put_Summary ("raw AET", RAET'Access);

   Put ("Starting raw AT... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Send (NS, SBuffer);
   end loop;
   After := Current;
   Put_Summary ("raw AT", RAT'Access);

   New_Line;

   Recept := SET - RSET;
   MarshUnmarsh2000 := ST - RST - Recept;
   MarshUnmarsh := MarshUnmarsh2000 / 2000.0;
   Setup := AET - RAET;
   Marsh1000 := ATT - RAT;
   Marsh := Marsh1000 / 1000.0;
   Unmarsh := MarshUnmarsh - Marsh;

   Sum ("Setup of an anonymous task", Recept);
   Sum ("Marshaling+unmarshaling of 2000 integers", MarshUnmarsh2000);
   Sum ("Marshaling+unmarshaling of one integer", MarshUnmarsh);
   Sum ("Setup of sender", Setup);
   Sum ("Marshaling of 1000 integers", Marsh1000);
   Sum ("Marshaling of one integer", Marsh);
   Sum ("Unmarshaling of one integer", Unmarsh);

end Local;
