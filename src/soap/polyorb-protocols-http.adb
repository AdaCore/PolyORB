with Ada.Characters;
with Ada.Characters.Handling;

with Ada.Strings.Fixed;  use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Streams;
--  with Ada.Exceptions; use Ada.Exceptions;

with PolyORB.Types;

with PolyORB.Buffers;
--   with PolyORB.Opaque;
with PolyORB.Representations.CDR;

with PolyORB.Utils.HTTP; use PolyORB.Utils.HTTP;
with PolyORB.Utils.HTTP_Messages; use PolyORB.Utils.HTTP_Messages;


--   with PolyORB.Protocols.SOAP;


--  with Interfaces;

package body  PolyORB.Protocols.HTTP is


   End_Section : constant String := "";

   --------------------------------
   ---  Utilities functions
   --------------------------------

   -------------
   -- Key_For --
   -------------

   function Key_For (Node : in Key_Value_Data) return String is
   begin
      return To_Standard_String (Node.Key);
   end Key_For;


   function Is_Match (Str, Pattern : in String) return Boolean;

   function Is_Match (Str, Pattern : in String) return Boolean is
      use Ada.Characters;
      U_Str     : constant String := Handling.To_Upper (Str);
      U_Pattern : constant String := Handling.To_Upper (Pattern);
   begin
      return Pattern'Length <= Str'Length
        and then U_Str (1 .. Pattern'Length) = U_Pattern;
   end Is_Match;

   -----------
   -- Image --
   -----------

   function Image (N : Natural) return String is
      N_Img : constant String := Natural'Image (N);
   begin
      return N_Img (N_Img'First + 1 .. N_Img'Last);
   end Image;


   -----------
   -- Buffer --
   -----------

   function To_Buffer (S : String)
      return Buffer_Access
   is
      use PolyORB.Representations.CDR;
      Buf : Buffer_Access := new Buffer_Type;
   begin
      for I in S'Range loop
         Marshall (Buf, PolyORB.Types.Char (S (I)));
         --  Car := PolyORB.Types.Char (S (I));
         --  Align_Marshall_Copy (Buffer, (1 => Stream_Element
         --    (PolyORB.Types.Octet'(PolyORB.Types.Char'Pos
         --   (Data)))), 1);
      end loop;
      return Buf;
   end To_Buffer;

   function URL_Parse (URL : in String) return URL_Object is

      HTTP_Token  : constant String := "http://";
      HTTPS_Token : constant String := "https://";

      O : URL_Object;

      procedure Parse (URL : in String);
      --  parse URL, the URL must not contain the HTTP_Token prefix.

      -----------
      -- Parse --
      -----------

      procedure Parse (URL : in String) is


         function US (S : in String)
                     return Unbounded_String
           renames To_Unbounded_String;

         I1, I2 : Natural;

      begin
         I1 := Index (URL, ":");
         I2 := Index (URL, "/");

         if I1 = 0 then
            O.Port := Default_Port;

            if I2 = 0 then
               O.Server_Name := Types.String (US (URL));
               O.URI         := Types.String (US ("/"));
            else
               O.Server_Name := Types.String (US (URL (URL'First .. I2 - 1)));
               O.URI         := Types.String (US (URL (I2 .. URL'Last)));
            end if;

         else

            O.Server_Name :=  Types.String (US (URL (URL'First .. I1 - 1)));

            if I2 = 0 then
               O.Port := Positive'Value (URL (I1 + 1 .. URL'Last));
               O.URI  := Types.String (US ("/"));
            else
               O.Port := Positive'Value (URL (I1 + 1 .. I2 - 1));
               O.URI  := Types.String (US (URL (I2 .. URL'Last)));
            end if;
         end if;
      end Parse;

   begin
      if Is_Match (URL, HTTP_Token) then
         Parse (URL (URL'First + HTTP_Token'Length .. URL'Last));
         O.Security := False;
      elsif Is_Match (URL, HTTPS_Token) then
         Parse (URL (URL'First + HTTPS_Token'Length .. URL'Last));
         O.Security := True;
      else
         Parse (URL);
         O.Security := False;
      end if;
      return O;
   exception
      when others =>
         raise URL_Error;
   end URL_Parse;

   function Get_URI
      (URL : URL_Object)
     return Types.String
   is
   begin
      return URL.URI;
   end Get_URI;


   ------------------
   -- Send_Command --
   ------------------

   procedure Send_Command
     (Connection : in out HTTP_Connection;
      Method     : in     String;
      URI        : in     String;
      Command    : out  Types.String);


   procedure Send_Command
     (Connection : in out HTTP_Connection;
      Method     : in     String;
      URI        : in     String;
      Command    : out Types.String)
   is

      use PolyORB.Buffers;

      function HTTP_Prefix (Security : in Boolean) return String;
      --  Returns "http://" or "https://" if Security is set to True.

      function Persistence return String;
      --  Returns "Keep-Alive" is we have a persistent connection and "Close"
      --  otherwise.

      function Port_Not_Default (Port : in Positive)
        return String;
      --  Returns the port image (preceded by character ':') if it is not the
      --  default port.

      ----------------------
      -- Port_Not_Default --
      ----------------------

      function Port_Not_Default
        (Port : in Positive)
        return String is
      begin
         if Port = 80 then
            return "";
         else
            declare
               Port_Image : constant String := Positive'Image (Port);
            begin
               return ':' & Port_Image (2 .. Port_Image'Last);
            end;
         end if;
      end Port_Not_Default;

      -----------------
      -- HTTP_Prefix --
      -----------------

      function HTTP_Prefix (Security : in Boolean) return String is
      begin
         if Security then
            return "https://";
         else
            return "http://";
         end if;
      end HTTP_Prefix;

      -----------------
      -- Persistence --
      -----------------

      function Persistence return String is
      begin
         if Connection.Persistent then
            return "Keep-Alive";
         else
            return "Close";
         end if;
      end Persistence;

      Host_Address : constant Types.String :=
          (Connection.Host_URL.Server_Name
           & Port_Not_Default (Connection.Host_URL.Port));

   begin
      Command := Null_Str;
      if Connection.Proxy = Null_Str then

         if URI = "" then
            Append (Command, Method & ' ' & To_Standard_String
               (Connection.Host_URL.URI) &
                ' ' & HTTP_Version & CRLF);

         else
            Append (Command, Method & ' ' & URI &
             ' ' & HTTP_Version & CRLF);

         end if;

         Append (Command, Connection_Token & Persistence);

      else
         if URI = "" then
            Append (Command, Method & ' ' &
               To_Standard_String (Connection.Host) &
               ' ' & HTTP_Version & CRLF);

         else
            Append (Command,
              Method & ' ' &  HTTP_Prefix (Connection.Host_URL.Security)
              & Host_Address & URI &
              ' ' & HTTP_Version & CRLF);
         end if;

         Append (Command, Proxy_Connection_Token & Persistence & CRLF);

      end if;

      Append (Command, Host_Token & Host_Address & CRLF);

      Append (Command, Accept_Token & "text/xml, */*" & CRLF);

      Append (Command, Accept_Language_Token & "fr, us" & CRLF);

      if Connection.User /= Null_Str
        and then Connection.Pwd /= Null_Str
      then
         Append (Command, Authorization_Token & "Basic" & ' ' &
            Base64_Encode
             (To_Standard_String (Connection.User)
              & ':' & To_Standard_String (Connection.Pwd)) & CRLF);
      end if;

      if Connection.Proxy_User /= Null_Str
        and then Connection.Proxy_Pwd /= Null_Str
      then
         Append (Command, Proxy_Authorization_Token & "Basic" & ' ' &
            Base64_Encode
             (To_Standard_String (Connection.Proxy_User)
              & ':' & To_Standard_String (Connection.Proxy_Pwd)) & CRLF);
      end if;
   end Send_Command;



   function Create_Connection
     (Host       : in String;
      User       : in String   := No_Data;
      Pwd        : in String   := No_Data;
      Proxy      : in String   := No_Data;
      Proxy_User : in String   := No_Data;
      Proxy_Pwd  : in String   := No_Data;
      Retry      : in Positive := Retry_Default;
      Persistent : in Boolean  := True)
      return HTTP_Connection
   is
      Connect_URL : URL_Object;
      Host_URL    : URL_Object := URL_Parse (Host);
      Proxy_URL   : URL_Object := URL_Parse (Proxy);
   begin
      if Proxy = No_Data then
         Connect_URL := Host_URL;
      else
         Connect_URL := Proxy_URL;
      end if;

      return (Connect_URL => Connect_URL,
              Host        => To_Unbounded_String (Host),
              Host_URL    => Host_URL,
              User        => To_Unbounded_String (User),
              Pwd         => To_Unbounded_String (Pwd),
              Proxy       => To_Unbounded_String (Proxy),
              Proxy_URL   => Proxy_URL,
              Proxy_User  => To_Unbounded_String (Proxy_User),
              Proxy_Pwd   => To_Unbounded_String (Proxy_Pwd),
              Opened      => True,
              Retry       => Create_Connection.Retry,
              Persistent  => Persistent);
   end Create_Connection;



   procedure Post
     (Connection : in out HTTP_Connection;
      Data       : in  Types.String;
      URI        : in  Types.String := Null_Str;
      Message    : out Types.String)
   is

      Command   : Types.String;
      --  Try_Count : Integer := Connection.Retry;
   begin
      Message := Null_Str;
      Send_Command (Connection, "POST", To_Standard_String (URI), Command);
      Append (Message, Command);
      Append (Message, Content_Type_Token & Text_XML & CRLF);

      --  Send message Content_Length
      Append (Message, Content_Length_Token & Natural'Image
                      (Length (Data)) & CRLF);

      Append (Message, CRLF);

      --  Send message body
      Append (Message, Data);

   end Post;


   procedure Answer_Client
    (Status   : Status_Code;
     Data     : Types.String;
     Message  : out Types.String)
   is

   begin
      Message := Null_Str;
      Append (Message, HTTP_Version & ' ' & To_PolyORB_String (Image (Status))
              & To_PolyORB_String (Reason_Phrase (Status)) & CRLF);

      Append (Message, Content_Type_Token & Text_XML & CRLF);

      --  Send message Content_Length
      Append (Message, Content_Length_Token & Natural'Image
                      (Length (Data)) & CRLF);

      Append (Message, CRLF);
      Append (Message, Data);

   end Answer_Client;


   --------------------------------------
   --  Host's URL from the Connection
   ---------------------------------------

   function Get_Host_URL
      (C : HTTP_Connection)
     return URL_Object
   is
   begin
      return C.Host_URL;
   end Get_Host_URL;




   ---------------------------------
   --  Parse Header of an HTTP Response
   -----------------------------------

   procedure Response_Parse_Header
     (Mess              :    in  Types.String;
      Param             :    HTTP_Response_Access)

   is

      Offset : Natural := 0;
      Str : Types.String;
      Index : Natural  := 1;
   begin
      --  Content_Length := 0;
      loop
         --   declare
         --   Line : constant String := Sockets.Get_Line (Sock);
         --   begin
         Offset := 0;
         if Index <= Length (Mess) then
            loop
               if Element (Mess, Index + Offset) = CR
                 and then Element (Mess, Index + Offset + 1) = LF then
                  Str := To_PolyORB_String (Slice (Mess,
                       Index, Index + Offset - 1));
                  Index := Index + Offset + 2;
                  exit;
               else
                  Offset := Offset + 1;
               end if;
            end loop;
         else
            exit;
         end if;

         declare
            Line : constant String := To_Standard_String (Str);
         begin

            if Is_Match (Line, HTTP_Token) then
               Param.Code := Status_Code'Value
                  ('S' & Line (HTTP_Token'Last + 5
                  .. HTTP_Token'Last + 7));

            elsif Is_Match (Line, Content_Type_Token) then
                  Param.Content_Type := To_Unbounded_String
                  (Line (Content_Type_Token'Last + 1 .. Line'
                     Last));

            elsif Is_Match (Line, Content_Length_Token) then
                  Param.Content_Length := Natural'Value
                  (Line (Content_Length_Range'Last + 1 .. Line'
                     Last));

            elsif Is_Match (Line, Location_Token) then
                  Param.Location := To_Unbounded_String
                  (Line (Location_Token'Last + 1 .. Line'Last));

            elsif Is_Match (Line, Transfer_Encoding_Token) then

                  Param.Transfer_Encoding := To_Unbounded_String
                    (Line (Transfer_Encoding_Range'Last + 1
                     .. Line'Last));

            elsif Is_Match (Line, Connection_Token) then
                  Param.Connection := To_Unbounded_String
                   (Line (Connection_Token'Last + 1 .. Line'Last));

            elsif Is_Match (Line, Proxy_Connection_Token) then
                  Param.Connection := To_Unbounded_String
                    (Line (Proxy_Connection_Token'Last + 1 .. Line'Last));

            else
               --  everything else is ignore right now
               null;
            end if;
         end;
      end loop;
   end Response_Parse_Header;

   procedure Request_Parse_Header
     (Mess    : in Types.String;
      Status  : HTTP_Request_Access;
      Success : out Boolean)
   is
      Offset : Natural := 0;
      Str : Types.String;
      Index  : Natural := 1;
   begin

      loop
         Offset := 0;
         if Index <= Length (Mess) then
            loop
               if Element (Mess, Index + Offset) = CR
                  and then Element (Mess, Index + Offset + 1) = LF then
                  Str := To_PolyORB_String (Slice (Mess, Index,
                            Index + Offset - 1));
                  Index := Index + Offset + 2;
                  exit;
               else
                  Offset := Offset + 1;
               end if;
            end loop;
            Request_Parse_Line (To_Standard_String (Str), Status, Success);
            if Success = False then
               exit;
            end if;
         else
            exit;
         end if;
      end loop;
   end Request_Parse_Header;


   procedure Request_Parse_Line
      (Command : String;
       Status  : HTTP_Request_Access;
       Success : out Boolean)
   is
      I1, I2 : Natural;
      --  Index of first space and second space

      I3 : Natural;
      --  Index of ? if present in the URI (means that there is some
      --  parameters)

      procedure Cut_Command;
      --  Parse Command and set I1, I2 and I3

      function URI return String;
      pragma Inline (URI);
      --  Returns first parameter. parameters are separated by spaces.

      --   function Parameters return String;
      --  Returns parameters if some where specified in the URI.

      function HTTP_Version return String;
      pragma Inline (HTTP_Version);
      --  Returns second parameter. parameters are separated by spaces.

      function Parse_Request_Line (Command : in String) return Request_Range;
      --  Parse the request line:
      --  Request-Line = Method SP Request-URI SP HTTP-Version CRLF

      -----------------
      -- Cut_Command --
      -----------------

      procedure Cut_Command is
      begin
         I1 := Ada.Strings.Fixed.Index (Command, " ");
         I2 := Ada.Strings.Fixed.Index (Command
               (I1 + 1 .. Command'Last), " ");
         I3 := Ada.Strings.Fixed.Index (Command (I1 + 1 .. I2), "?");
      end Cut_Command;

      ---------
      -- URI --
      ---------

      function URI return String is
      begin
         if I3 = 0 then
            return Command (I1 + 1 .. I2 - 1);
         else
            return Command (I1 + 1 .. I3 - 1);
         end if;
      end URI;

      ------------------
      -- HTTP_Version --
      ------------------

      function HTTP_Version return String is
      begin
         return Command (I2 + 1 .. Command'Last);
      end HTTP_Version;


      ------------------------
      -- Parse_Request_Line --
      ------------------------

      function Parse_Request_Line (Command : in String) return Request_Range is
      begin
         Cut_Command;

         if Is_Match (Command, Post_Token) then
            Status.Method := POST;
            Status.URI := To_PolyORB_String (URI);
            Status.HTTP_Version := To_PolyORB_String (HTTP_Version);
            return POST;

         elsif Is_Match (Command, Post_Token)
               or Is_Match (Command, Head_Token) then
            return NON_POST;
         else
            return EMPTY;
         end if;
      end Parse_Request_Line;

   begin
      Success := True;
      if Parse_Request_Line (Command) = NON_POST then
         Success := False;

      elsif Is_Match (Command, Host_Token) then
         Status.Host :=
            To_PolyORB_String (Command (Host_Token'Length +
                                                1 .. Command'Last));

      elsif  Is_Match (Command, Connection_Token) then
         Status.Connection :=
            To_PolyORB_String (Command (Connection_Token'Length +
                                                1 .. Command'Last));

      elsif Is_Match (Command, Content_Length_Token) then
         Status.Content_Length := Natural'Value
            (Command (Content_Length_Token'Length + 1
             .. Command'Last));

      elsif Is_Match (Command, Content_Type_Token) then
         declare
            Pos : constant Natural := Ada.Strings.Fixed.Index (Command, ";");
         begin
            if Pos = 0 then
               Status.Content_Type := To_PolyORB_String (Command
                  (Content_Type_Token'Length + 1 .. Command'Last));
            else
               Status.Content_Type := To_PolyORB_String (
                  Command (Content_Type_Token'Length + 1 .. Pos - 1));
               Status.Boundary := To_PolyORB_String (
                  Command (Pos + 11 .. Command'Last));
            end if;
         end;

      elsif Is_Match
        (Command, If_Modified_Since_Token)
      then
         Status.If_Modified_Since := To_PolyORB_String (
            Command (If_Modified_Since_Token'Length + 1
                     .. Command'Last));


      end if;

   exception
      when others =>
         raise Internal_Error;

   end Request_Parse_Line;

   ----------------------------
   --  Utility functions
   ---------------------------

   function Response_Status
     (Resp : HTTP_Response)
      return Status_Code
   is
   begin
      return Resp.Code;
   end Response_Status;

   function Response_Body
     (Resp : HTTP_Response)
     return Types.String
   is
   begin
      return Resp.Message_Body;
   end Response_Body;

   function Response_CT
     (Resp : HTTP_Response)
     return Types.String
   is
   begin
      return Resp.Content_Type;
   end Response_CT;


   function Response_TE
     (Resp : HTTP_Response)
     return Types.String
   is
   begin
      return Resp.Transfer_Encoding;
   end Response_TE;

   function Request_Mtd
     (Req : HTTP_Request)
     return Request_Method
   is
   begin
      return Req.Method;
   end Request_Mtd;


   function Request_Version
     (Req : HTTP_Request)
     return Types.String
   is
   begin
      return Req.HTTP_Version;
   end Request_Version;


   function Request_CT
     (Req : HTTP_Request)
     return Types.String
   is
   begin
      return Req.Content_Type;
   end Request_CT;

   function Request_URI (Req : HTTP_Request)
     return Types.String
   is
   begin
      return Req.URI;
   end Request_URI;



end PolyORB.Protocols.HTTP;
