--
--  $Id$
--

with Ada.Streams;                     use Ada.Streams;
with Interfaces.C;                    use Interfaces.C;
with System.Garlic.Constants;         use System.Garlic.Constants;
with System.Garlic.Naming;            use System.Garlic.Naming;
with System.Garlic.Network_Utilities; use System.Garlic.Network_Utilities;
with System.Garlic.Partitions;        use System.Garlic.Partitions;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Streams;           use System.Garlic.Streams;
with System.Garlic.Thin;              use System.Garlic.Thin;
with System.Garlic.Types;             use System.Garlic.Types;
with System.Garlic.Utils;             use System.Garlic.Utils;
with Utils;                           use Utils;

package body Remote is

   SBuffer   : Stream_Element_Access;
   OneBuffer : Stream_Element_Access;

   function Get_Host (S : String) return String;

   -----------------------------
   -- Asynchronous_Empty_Test --
   -----------------------------

   procedure Asynchronous_Empty_Test is
   begin
      null;
   end Asynchronous_Empty_Test;

   -----------------------
   -- Asynchronous_Test --
   -----------------------

   procedure Asynchronous_Test (A : in T) is
   begin
      null;
   end Asynchronous_Test;

   ---------------------
   -- Enter_Test_Mode --
   ---------------------

   procedure Enter_Test_Mode
     (Partition : in Natural;
      Port      : in Positive;
      Tries     : in Natural)
   is
      Location : Location_Type;
      Error    : Error_Type;
      Name     : aliased Sockaddr_In;
      Sock     : int;
   begin
      OneBuffer := new Stream_Element_Array (1 .. 1);
      SBuffer   := new Stream_Element_Array (1 .. T'Size / 8);
      Get_Location (Partition_ID (Partition), Location, Error);
      Name.Sin_Addr :=
        To_In_Addr (Address_Of (Get_Host (Get_Data (Location).all)));
      Name.Sin_Port := Port_To_Network (unsigned_short (Port));
      Sock := C_Socket (Pf_Inet, Sock_Stream, 0);
      if C_Connect (Sock, Name'Address, Name'Size / 8) /= 0 then
         return;
      end if;

      for I in 1 .. Tries loop
         Receive (Sock, OneBuffer);
         Send (Sock, OneBuffer);
      end loop;

      for I in 1 .. Tries loop
         Receive (Sock, SBuffer);
         Send (Sock, SBuffer);
      end loop;

      for I in 1 .. Tries loop
         Receive (Sock, OneBuffer);
      end loop;

      for I in 1 .. Tries loop
         Receive (Sock, SBuffer);
      end loop;
   end Enter_Test_Mode;

   --------------
   -- Get_Host --
   --------------

   function Get_Host (S : String) return String is
   begin
      for I in S'Range loop
         if S (I) = ':' then
            return S (S'First .. I-1);
         end if;
      end loop;
      return S;
   end Get_Host;

   ----------------------------
   -- Synchronous_Empty_Test --
   ----------------------------

   procedure Synchronous_Empty_Test is
   begin
      null;
   end Synchronous_Empty_Test;

   ----------------------
   -- Synchronous_Test --
   ----------------------

   procedure Synchronous_Test (A : in out T) is
   begin
      null;
   end Synchronous_Test;

end Remote;
