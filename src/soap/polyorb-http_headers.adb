--  THIS IS A GENERATED FILE, DO NOT EDIT!
with Interfaces; use Interfaces;

with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Utils.Strings;

package body PolyORB.HTTP_Headers is

   type String_Access is access String;

   Table : array (Header) of String_Access;

   function Hash (S : String) return Natural;
   pragma Inline (Hash);

   procedure Initialize;

   procedure Set (N : Header; S : String);

   P : constant array (0 .. 4) of Natural :=
     (2, 4, 9, 10, 12);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (90, 40, 93, 89, 32);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (25, 56, 31, 77, 92);

   G : constant array (0 .. 95) of Unsigned_8 :=
     (0, 0, 0, 10, 0, 0, 0, 0, 0, 42, 0, 0, 0, 35, 0, 0, 0, 2, 0, 32, 0, 16,
      0, 0, 44, 40, 0, 38, 14, 0, 3, 0, 0, 25, 23, 0, 0, 0, 24, 1, 26, 0, 29,
      0, 21, 0, 0, 30, 0, 27, 17, 23, 0, 0, 0, 38, 13, 0, 5, 43, 0, 0, 13, 0,
      6, 21, 0, 0, 16, 0, 0, 0, 0, 0, 8, 0, 42, 0, 16, 28, 19, 11, 45, 0, 36,
      0, 9, 43, 0, 0, 4, 0, 23, 25, 31, 20);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for I in P'Range loop
         exit when L < P (I);
         J  := Character'Pos (S (P (I) + F));
         F1 := (F1 + Natural (T1 (I)) * J) mod 96;
         F2 := (F2 + Natural (T2 (I)) * J) mod 96;
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
      (Name => +"http_headers",
       Conflicts => Empty,
       Depends => Empty,
       Provides => Empty,
       Init => Initialize'Access));
end PolyORB.HTTP_Headers;
