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

   procedure Put_Summary (T : in String);

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

   procedure Put_Summary (T : in String) is
   begin
      Put_Line ("done");
      Put ("                   " & T & " summary:");
      Put (Float (After - Before) / Float (Iterations), Fore => 3, Aft => 2,
           Exp => 0);
      Put_Line ("ms");
   end Put_Summary;

   S   : aliased int;
   NS  : int;
   NA  : aliased Sockaddr_In;
   NAL : aliased int := NA'Size / 8;

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
   Put_Summary ("SET");

   Put ("Starting ST... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Synchronous_Test (Buffer);
   end loop;
   After := Current;
   Put_Summary ("ST");

   Put ("Starting AET... "); Flush;
   Before := Current;
   for I in 1 .. Iterations loop
      Asynchronous_Empty_Test;
   end loop;
   After := Current;
   Put_Summary ("AET");

   Put ("Starting AT... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Asynchronous_Test (Buffer);
   end loop;
   After := Current;
   Put_Summary ("AT");

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
   Put_Summary ("raw SET");

   Put ("Starting raw ST... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Send (NS, SBuffer);
      Receive (NS, SBuffer);
   end loop;
   After := Current;
   Put_Summary ("raw ST");

   Put ("Starting raw AET... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Send (NS, OneBuffer);
   end loop;
   After := Current;
   Put_Summary ("raw AET");

   Put ("Starting raw AT... ");
   Before := Current;
   for I in 1 .. Iterations loop
      Send (NS, SBuffer);
   end loop;
   After := Current;
   Put_Summary ("raw AT");

end Local;
