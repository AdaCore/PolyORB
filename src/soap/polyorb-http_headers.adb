--  THIS IS A GENERATED FILE, DO NOT EDIT!
with Interfaces; use Interfaces;

with PolyORB.Initialization;

with PolyORB.Utils.Strings;

package body PolyORB.HTTP_Headers is

   type String_Access is access String;

   Table : array (Header) of String_Access;

   function Hash (S : String) return Natural;
   pragma Inline (Hash);

   procedure Initialize;

   procedure Set (N : Header; S : String);

   P : constant array (0 .. 3) of Natural :=
     (2, 4, 10, 13);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (28, 62, 54, 61);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (20, 37, 10, 27);

   G : constant array (0 .. 94) of Unsigned_8 :=
     (38, 33, 12, 0, 0, 0, 0, 0, 10, 0, 35, 0, 10, 0, 18, 3, 0, 9, 0, 2, 0,
      0, 23, 0, 0, 0, 0, 41, 13, 0, 21, 0, 3, 0, 23, 34, 8, 0, 0, 0, 28, 0,
      32, 0, 42, 0, 43, 0, 0, 31, 22, 0, 34, 0, 0, 19, 0, 19, 0, 13, 0, 0,
      0, 0, 0, 5, 0, 41, 0, 6, 14, 0, 2, 0, 33, 1, 23, 23, 3, 0, 0, 0, 0, 0,
      5, 41, 11, 0, 12, 0, 19, 0, 36, 6, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 95;
         F2 := (F2 + Natural (T2 (K)) * J) mod 95;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 47;
   end Hash;

   function In_Word_Set (S : String) return Header is
      N : constant Header := Header'Val (Hash (S));
   begin
      if Table (N).all = S then
         return N;
      else
         return Extension_Header;
      end if;
   end In_Word_Set;

   procedure Initialize is
   begin
      Set (H_Cache_Control, "Cache-Control");
      Set (H_Connection, "Connection");
      Set (H_Date, "Date");
      Set (H_Pragma, "Pragma");
      Set (H_Trailer, "Trailer");
      Set (H_Transfer_Encoding, "Transfer-Encoding");
      Set (H_Upgrade, "Upgrade");
      Set (H_Via, "Via");
      Set (H_Warning, "Warning");
      Set (H_Accept, "Accept");
      Set (H_Accept_Charset, "Accept-Charset");
      Set (H_Accept_Language, "Accept-Language");
      Set (H_Authorization, "Authorization");
      Set (H_Expect, "Expect");
      Set (H_From, "From");
      Set (H_Host, "Host");
      Set (H_If_Match, "If-Match");
      Set (H_If_Modified_Since, "If-Modified-Since");
      Set (H_If_None_Match, "If-None-Match");
      Set (H_If_Range, "If-Range");
      Set (H_If_Unmodified_Since, "If-Unmodified-Since");
      Set (H_Max_Forwards, "Max-Forwards");
      Set (H_Proxy_Authorization, "Proxy-Authorization");
      Set (H_Range, "Range");
      Set (H_Referer, "Referer");
      Set (H_TE, "TE");
      Set (H_User_Agent, "User-Agent");
      Set (H_Accept_Ranges, "Accept-Ranges");
      Set (H_Age, "Age");
      Set (H_ETag, "ETag");
      Set (H_Location, "Location");
      Set (H_Proxy_Authenticate, "Proxy-Authenticate");
      Set (H_Retry_After, "Retry-After");
      Set (H_Server, "Server");
      Set (H_Vary, "Vary");
      Set (H_WWW_Authenticate, "WWW-Authenticate");
      Set (H_Allow, "Allow");
      Set (H_Content_Encoding, "Content-Encoding");
      Set (H_Content_Language, "Content-Language");
      Set (H_Content_Length, "Content-Length");
      Set (H_Content_Location, "Content-Location");
      Set (H_Content_MD5, "Content-MD5");
      Set (H_Content_Range, "Content-Range");
      Set (H_Content_Type, "Content-Type");
      Set (H_Expires, "Expires");
      Set (H_Last_Modified, "Last-Modified");
      Set (H_SOAPAction, "SOAPAction");
   end Initialize;

   procedure Set (N : Header; S : String) is
   begin
      Table (N) := new String'(S);
   end Set;

   function To_String (Id : Header) return String is
   begin
      return Table (Id).all;
   end To_String;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"http_headers",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.HTTP_Headers;
