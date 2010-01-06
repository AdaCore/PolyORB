------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           A W S . C L I E N T                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2009, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Requests;
with PolyORB.References.URI;
with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Types;
with PolyORB.Log;

with AWS.Headers.Set;
with AWS.Messages;
with AWS.Response.Set;
with AWS.Translator;

package body AWS.Client is

   use Ada;

   use PolyORB.Log;
   package L is
     new PolyORB.Log.Facility_Log ("aws.web_client");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

--     type Auth_Attempts_Count is
--       array (Authentication_Level) of Natural range 0 .. 2;

   Debug_On : Boolean := False;

--    procedure Debug_Message (Prefix, Message : String);
--    pragma Inline (Debug_Message);
   --  Output Message prefixed with Prefix if Debug_On is True and does
   --  nothing otherwise.

   procedure Handle_Request
     (Connection : in out HTTP_Connection;
      Method     : String;
      Parameters : Unbounded_String;
      Result     :    out Response.Data);
   --  creates a PolyORB neutral request from parameters, sends it and
   --  fill in Result with the response

--     procedure Get_Response
--       (Connection : in out HTTP_Connection;
--        Result     :    out Response.Data;
--        Get_Body   : Boolean         := True);
   --  Receives response from server for GET and POST and HEAD commands.
   --  If Get_Body is set then the message body will be read.

--     procedure Decrement_Authentication_Attempt
--       (Connection : in out HTTP_Connection;
--        Counter    : in out Auth_Attempts_Count;
--        Over       :    out Boolean);
   --  Counts the authentication attempts. Over is set to True when
   --  authentication attempts are over.

   procedure Set_Authentication
     (Auth       :    out Authentication_Type;
      User       : String;
      Pwd        : String;
      Mode       : Authentication_Mode);
   --  Internal procedure to set authentication parameters.

--   procedure Parse_Header
--     (Connection        : in out HTTP_Connection;
--      Answer            :    out Response.Data;
--     Keep_Alive        :    out Boolean);
   --  Read server answer and set corresponding variable with the value
   --  read. Most of the fields are ignored right now.

   procedure Connect (Connection : in out HTTP_Connection);
   --  Open the connection. Raises Connection_Error if it is not possible to
   --  establish the connection. In this case all resources are released and
   --  Connection.Opened is set to False.

   procedure Disconnect (Connection : in out HTTP_Connection);
   --  Close connection. Further use is not possible.

--     procedure Open_Send_Common_Header
--       (Connection : in out HTTP_Connection;
--        Method     : String;
--        URI        : String);
   --  Open the the Connection if it is not open. Send the common HTTP headers
   --  for all requests like the proxy, authentification, user agent, host.

   procedure Set_Phase
     (Connection : in out HTTP_Connection;
      Phase      : Client_Phase);
   pragma Inline (Set_Phase);
   --  Set the phase for the connection. This will activate the Send and
   --  Receive timeouts of the cleaner task if needed.

--     procedure Send_Header
--       (Sock : Net.Socket_Type'Class;
--        Data : String);
--     pragma Inline (Send_Header);
   --  Send header Data to socket and call Debug_Message.

   ------------------
   -- Cleaner_Task --
   ------------------

--     task body Cleaner_Task is
--        Connection : HTTP_Connection_Access;
--        Forever    : constant Duration := Duration'Last;
--        P          : Client_Phase      := Not_Monitored;
--        W          : Duration;
--        Timeout    : Boolean;
--     begin
--        accept Start (Connection : HTTP_Connection_Access) do
--           Cleaner_Task.Connection := Connection;
--        end Start;

--        Phase_Loop : loop

--           --  Wait for the job to be done

--           case P is
--              when Stopped =>
--                 exit Phase_Loop;

--              when Not_Monitored =>
--                 W := Forever;

--              when Receive =>
--                 W := Duration (Connection.Timeouts.Receive);

--              when Send =>
--                 W := Duration (Connection.Timeouts.Send);
--           end case;

--           if W = 0.0 then
--              P := Not_Monitored;
--              W := Forever;
--           end if;

--           select
--              accept Next_Phase do
--                 P       := Connection.Current_Phase;
--                 Timeout := False;
--              end Next_Phase;
--           or
--              delay W;

--              Timeout := True;
--           end select;

--           --  Still in the same phase after the delay, just close the socket
--           --  now.

--           if Timeout
--             and then P /= Not_Monitored
--             and then Connection.Opened
--           then
--              Disconnect (Connection.all);
--           end if;

--        end loop Phase_Loop;

--     exception
--        when E : others =>
--           Text_IO.Put_Line (Exceptions.Exception_Information (E));
--     end Cleaner_Task;

   -----------
   -- Close --
   -----------

   procedure Close (Connection : in out HTTP_Connection) is

--      procedure Free is new Ada.Unchecked_Deallocation
--        (Cleaner_Task, Cleaner_Access);

   begin
      Connection.Current_Phase := Stopped;

--        if Connection.Cleaner /= null then

--           begin
--        --  We don't want to fail here, we really want to free the cleaner
--              --  object.
--              if not Connection.Cleaner'Terminated then
--                 Connection.Cleaner.Next_Phase;
--              end if;
--           exception
--              when others =>
--                 null;
--           end;

--           while not Connection.Cleaner'Terminated loop
--              delay 0.01;
--           end loop;

--           Free (Connection.Cleaner);

--        end if;

      Disconnect (Connection);
--      Net.Free (Connection.Socket);
   end Close;

   -------------
   -- Connect --
   -------------

   procedure Connect (Connection : in out HTTP_Connection) is
--        use type Net.Socket_Access;
--        Connect_URL : AWS.URL.Object renames Connection.Connect_URL;
   begin
      null;
      --        pragma Assert (not Connection.Opened);
      --        --  This should never be called with an open connection.

--    --  Keep-alive reconnection will be with old socket. We cannot reuse it,
--        --  and have to free it.

--        if Connection.Socket /= null then
--           Net.Free (Connection.Socket);
--        end if;

--        Connection.Socket := Net.Socket (AWS.URL.Security (Connect_URL));

--        Net.Connect (Connection.Socket.all,
--                     AWS.URL.Host (Connect_URL),
--                     AWS.URL.Port (Connect_URL));

      Connection.Opened := True;
--     exception
--        when E : Net.Socket_Error =>
--           Connection.Opened := False;

--           Exceptions.Raise_Exception
--             (Connection_Error'Identity,
--              "can't connect to " & AWS.URL.URL (Connect_URL)
--                & " -> " & Exceptions.Exception_Information (E));
   end Connect;

   -----------------
   -- Copy_Cookie --
   -----------------

   procedure Copy_Cookie
     (Source      : HTTP_Connection;
      Destination : in out HTTP_Connection) is
   begin
      Destination.Cookie := Source.Cookie;
   end Copy_Cookie;

   ------------
   -- Create --
   ------------

   procedure Create
     (Connection  : in out HTTP_Connection;
      Host        : String;
      User        : String          := No_Data;
      Pwd         : String          := No_Data;
      Proxy       : String          := No_Data;
      Proxy_User  : String          := No_Data;
      Proxy_Pwd   : String          := No_Data;
      Retry       : Natural         := Retry_Default;
      SOAPAction  : String          := No_Data;
      Persistent  : Boolean         := True;
      Timeouts    : Timeouts_Values := No_Timeout;
      Server_Push : Boolean         := False)
   is
      function Set (V : String) return Unbounded_String;
      --  Returns V as an Unbounded_String if V is not the empty string
      --  otherwise it returns Null_Unbounded_String.

      ---------
      -- Set --
      ---------

      function Set (V : String) return Unbounded_String is
      begin
         if V = No_Data then
            return Null_Unbounded_String;
         else
            return To_Unbounded_String (V);
         end if;
      end Set;

      Connect_URL : AWS.URL.Object;
      Host_URL    : AWS.URL.Object := AWS.URL.Parse (Host);
      Proxy_URL   : AWS.URL.Object := AWS.URL.Parse (Proxy);

   begin
      --  If there is a proxy, the host to connect to is the proxy otherwise
      --  we connect to the Web server.

      if Proxy = No_Data then
         Connect_URL := Host_URL;
      else
         Connect_URL := Proxy_URL;
      end if;

      Connection.Host                     := To_Unbounded_String (Host);
      Connection.Host_URL                 := Host_URL;
      Connection.Connect_URL              := Connect_URL;
      Connection.Auth (WWW).User          := Set (User);
      Connection.Auth (WWW).Pwd           := Set (Pwd);
      Connection.Proxy                    := Set (Proxy);
      Connection.Proxy_URL                := Proxy_URL;
      Connection.Auth (Client.Proxy).User := Set (Proxy_User);
      Connection.Auth (Client.Proxy).Pwd  := Set (Proxy_Pwd);
      Connection.Retry                    := Create.Retry;
      Connection.Cookie                   := Null_Unbounded_String;
      Connection.SOAPAction               := Set (SOAPAction);
      Connection.Persistent               := Persistent;
      Connection.Current_Phase            := Not_Monitored;
      Connection.Server_Push              := Server_Push;
      Connection.Timeouts                 := Timeouts;

      --  Establish the connection now

      Connect (Connection);

      if Persistent and then Connection.Retry = 0 then
         --  In this case the connection termination can be initiated by the
         --  server or the client after a period. So the connection could be
         --  closed while trying to get some data from the server. To be nicer
         --  from user's point of view just make sure we retry at least one
         --  time before reporting an error.
         Connection.Retry := 1;
      end if;

      if Connection.Timeouts /= No_Timeout then
         null;
         --  If we have some timeouts, initialize the cleaner task.
--         Connection.Cleaner := new Cleaner_Task;
--         Connection.Cleaner.Start (Connection.Self);
      end if;
   end Create;

   -------------------
   -- Debug_Message --
   -------------------

--    procedure Debug_Message (Prefix, Message : String) is
--    begin
--       if Debug_On then
--          Text_IO.Put_Line (Prefix & Message);
--       end if;
--    end Debug_Message;

   --------------------------------------
   -- Decrement_Authentication_Attempt --
   --------------------------------------

--     procedure Decrement_Authentication_Attempt
--       (Connection : in out HTTP_Connection;
--        Counter    : in out Auth_Attempts_Count;
--        Over       :    out Boolean)
--     is
--        type Over_Data is array (Authentication_Level) of Boolean;

--        Is_Over    : constant Over_Data := (others => True);
--        Over_Level : Over_Data          := (others => True);
--     begin
--        for Level in Authentication_Level'Range loop
--           if Connection.Auth (Level).Requested then
--              Counter (Level)    := Counter (Level) - 1;
--              Over_Level (Level) := Counter (Level) = 0;
--           end if;
--        end loop;

--        Over := Over_Level = Is_Over;
--     end Decrement_Authentication_Attempt;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Connection : in out HTTP_Connection) is
--      use type Net.Socket_Access;
   begin
      if Connection.Opened then
         Connection.Opened := False;

--         if Connection.Socket /= null then
--            Net.Shutdown (Connection.Socket.all);
--         end if;
      end if;
   end Disconnect;

   --------------------
   -- Handle_Request --
   --------------------

   procedure Handle_Request
     (Connection : in out HTTP_Connection;
      Method     : String;
      Parameters : Unbounded_String;
      Result     :    out Response.Data)
   is
      use PolyORB.Any.NVList;
      use PolyORB.Any;
      use PolyORB.Any.TypeCode;
      use PolyORB.Types;
      use PolyORB.References;
      use PolyORB.Requests;
      use AWS.URL;

      Args : PolyORB.Any.NVList.Ref;
      PolyORB_Request : PolyORB.Requests.Request_Access;
      PolyORB_Response : PolyORB.Any.NamedValue;
      Reference : constant String :=
        Protocol (Connection.Host_URL)
        & "://"
        & Host (Connection.Host_URL)
        & ":" & Port (Connection.Host_URL)
        & Path (Connection.Host_URL)
        & File (Connection.Host_URL);
   begin

      pragma Debug (C, O ("Handle_Request: building a "
                       & Method & " request"));
      pragma Debug (C, O ("Handle_Request: Reference is " & Reference));

      Create (Args);
      declare
         use AWS.Translator;

         Eq_Idx, Amp_Idx : Natural;
         Params : Unbounded_String := Parameters;
      begin
         --  Params is supposed to be like this :
         --  param=value&param=value...

         Append (Params, "&");
         --  here we add an extra ampersand at then of the parameters
         --  string in order to handle uniform pattern for parameters:
         --  name=value&

         pragma Debug (C, O ("Handle_Request: parameter string is <"
                          & To_String (Params) & ">"));

         while Length (Params) > 1 loop
            Eq_Idx := Ada.Strings.Unbounded.Index (Params, "=");
            Amp_Idx := Ada.Strings.Unbounded.Index (Params, "&");

            pragma Debug (C, O ("Handle_Request: parameter name: "
                             & (Slice (Params, 1, Eq_Idx - 1))));
            pragma Debug (C, O ("Handle_Request: parameter val.: "
                             & (Slice (Params, Eq_Idx + 1, Amp_Idx - 1))));

            Add_Item (Args,
                      (Name => To_PolyORB_String
                       (Slice (Params, 1, Eq_Idx - 1)),
                       Argument =>
                         To_Any (To_PolyORB_String
                                 (Slice (Params, Eq_Idx + 1, Amp_Idx - 1))),
                       Arg_Modes => PolyORB.Any.ARG_IN));

            Delete (Params, 1, Amp_Idx);
            --  we purge the parameter that has just been read

            if Amp_Idx = 0 then
               raise URL_Error;
               --  this is not supposed to happen, as we added an
               --  extra '&' at the end of the parameters. But then we
               --  would get an infinite loop, since Delete (Params,
               --  1, 0) does not delete anything
            end if;

         end loop;
         --  we parse each parameter line to retreive the name
         --  and the content of the parameters, and then we
         --  create an any of typecode string

         pragma Debug (C, O ("Handle_Request: parameter string is now <"
                          & To_String (Params) & ">"));

      end;

      Create_Request (Target => PolyORB.References.URI.String_To_Object
                      (Reference),
                      Operation => Method,
                      Arg_List => Args,
                      Result => PolyORB_Response,
                      Req => PolyORB_Request);

      Invoke (PolyORB_Request);

      pragma Debug (C, O ("Type of response is " & Image
                       (Get_Unwound_Type
                        (PolyORB_Request.Result.Argument))));

      declare
         Kind_Of_Result : constant PolyORB.Any.TCKind :=
           Kind (Get_Unwound_Type (PolyORB_Request.Result.Argument));
      begin
         if Kind_Of_Result = Tk_String then

            --  Typical situation: we get an html page, or any kind of
            --  text.

            Response.Set.Message_Body
              (Result, To_Standard_String
               (From_Any (PolyORB_Request.Result.Argument)));

         elsif Kind_Of_Result = Tk_Sequence
           and then  PolyORB.Any.TypeCode.Kind
           (PolyORB.Any.TypeCode.Content_Type
            (PolyORB.Any.Get_Type (PolyORB_Request.Result.Argument)))
           /= Tk_Octet
         then

            --  If we get a binary file, e.g. an image.  That is to
            --  say a sequence of octets.

            declare
               use Ada.Streams;

               Number_Of_Elements : constant Unsigned_Long :=
                 PolyORB.Any.Get_Aggregate_Count
                 (PolyORB_Request.Result.Argument);
               Byte_Stream : Stream_Element_Array
                 (0 .. Stream_Element_Offset (Number_Of_Elements) - 1);

            begin
               pragma Debug (C, O ("Handle_Request: Tk_Sequence: "
                                & "attempting to retrieve"
                                & Unsigned_Long'Image (Number_Of_Elements)
                                & " bytes"));

               for Index in 0 .. Number_Of_Elements - 1 loop
                  declare
                     Element : constant PolyORB.Types.Octet
                       := PolyORB.Any.From_Any
                       (PolyORB.Any.Get_Aggregate_Element
                        (PolyORB_Request.Result.Argument,
                         PolyORB.Any.TypeCode.TC_Octet,
                         Index));
                  begin
                     Byte_Stream (Stream_Element_Offset (Index))
                       := Stream_Element (Element);
                  end;
               end loop;
               Response.Set.Message_Body (Result, Byte_Stream);
            end;
         else
            raise Program_Error;
            --  If we get an unhandled type, we fail.
         end if;
      end;
   end Handle_Request;

   ---------
   -- Get --
   ---------

   function Get
     (URL                : String;
      User               : String          := No_Data;
      Pwd                : String          := No_Data;
      Proxy              : String          := No_Data;
      Proxy_User         : String          := No_Data;
      Proxy_Pwd          : String          := No_Data;
      Timeouts           : Timeouts_Values := No_Timeout;
      Follow_Redirection : Boolean         := False)
      return Response.Data
   is
      use type Messages.Status_Code;

      Result : Response.Data;

   begin
      declare
         Connection : HTTP_Connection;
      begin
         Create (Connection,
                 URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
                 Persistent => False,
                 Timeouts   => Timeouts);

         Get (Connection, Result);

         Close (Connection);
      exception
         when others =>
            Close (Connection);
            raise;
      end;

      if Follow_Redirection
        and then Response.Status_Code (Result) = Messages.S305
      then
         --  This is "Use Proxy" message, Location point to the proxy to use.
         --  We do not have the login/password for the proxy.
         return Get
           (URL, User, Pwd, Response.Location (Result),
            Timeouts => Timeouts, Follow_Redirection => Follow_Redirection);

      elsif Follow_Redirection
          and then
        Response.Status_Code (Result) in Messages.S301 .. Messages.S307
          and then
        Response.Status_Code (Result) /= Messages.S304
      then
         --  All other redirections, 304 is not one of them.
         return Get
           (Response.Location (Result), User, Pwd,
            Proxy, Proxy_User, Proxy_Pwd, Timeouts, Follow_Redirection);
      else
         return Result;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      URI        : String          := No_Data)
   is
      --  this is the main 'get' function. All other get functions call
      --  this one

      use AWS.URL;

   begin

      if URI /= No_Data then
         declare
            Overriding_URI : URL.Object := Parse (URI);
            Parameters : constant Unbounded_String :=
              To_Unbounded_String (URL.Query (Overriding_URI));
         begin
            Connection.Host_URL := Overriding_URI;
            Connection.Host := To_Unbounded_String (Host (Overriding_URI));

            Handle_Request (Connection, "GET", Parameters, Result);
         end;
      else
         declare
            Parameters : constant Unbounded_String :=
              To_Unbounded_String (URL.Query (Connection.Host_URL));
         begin
            Handle_Request (Connection, "GET", Parameters, Result);
         end;
      end if;

   end Get;

   ------------------
   -- Get_Response --
   ------------------

--     procedure Get_Response
--       (Connection : in out HTTP_Connection;
--        Result     :    out Response.Data;
--        Get_Body   : Boolean         := True)
--     is

--    subtype Stream_Element_Array_Access is Utils.Stream_Element_Array_Access;

--        function Read_Chunk return Stream_Element_Array_Access;
--        --  Read a chunk object from the stream

--        function Read_Binary_Message
--          (Len : Positive)
--           return Stream_Element_Array_Access;
--        pragma Inline (Read_Binary_Message);
--        --  Read a binary message of Len bytes from the socket.

--        procedure Disconnect;
--        --  close connection socket.

--  --      Sock       : Net.Socket_Type'Class renames Connection.Socket.all;

--        Keep_Alive : constant Boolean := False;

--        ----------------
--        -- Disconnect --
--        ----------------

--        procedure Disconnect is
--        begin
--           if not Keep_Alive and not Connection.Server_Push then
--              Disconnect (Connection);
--           end if;
--        end Disconnect;

--        -------------------------
--        -- Read_Binary_Message --
--        -------------------------

--        function Read_Binary_Message
--          (Len : Positive)
--           return Stream_Element_Array_Access
--        is
--           use Streams;

--           Elements : Stream_Element_Array_Access
--             := new Stream_Element_Array (1 .. Stream_Element_Offset (Len));
--           S, E     : Stream_Element_Offset;
--        begin
--           S := 1;

--           --  Read the message, 10k at a time

--           loop
--              E := Stream_Element_Offset'Min
--                (Stream_Element_Offset (Len), S + 10_239);

--  --            Net.Buffered.Read (Sock, Elements (S .. E));

--              S := E + 1;

--              exit when S > Stream_Element_Offset (Len);
--           end loop;

--           return Elements;

--        exception
--  --         when Net.Socket_Error =>
--           when others =>
--              --  Could have been killed by a timeout.
--              Utils.Free (Elements);
--              raise;
--        end Read_Binary_Message;

--        ----------------
--        -- Read_Chunk --
--        ----------------

--        function Read_Chunk return Stream_Element_Array_Access is

--           use Streams;

--           use type Stream_Element_Array;
--           use type Stream_Element_Offset;

--           procedure Skip_Line;
--           --  skip a line on the socket

--           Data      : Stream_Element_Array_Access
--             := new Streams.Stream_Element_Array (1 .. 10_000);

--           Data_Last : Streams.Stream_Element_Offset := 0;

--           ---------------
--           -- Skip_Line --
--           ---------------

--           procedure Skip_Line is
--  --            D : constant String := Net.Buffered.Get_Line (Sock);
--              D : constant String := "toto";
--              pragma Warnings (Off, D);
--           begin
--              null;
--           end Skip_Line;

--           Size : Stream_Element_Offset;
--           Tmp  : Stream_Element_Array_Access;

--        begin
--           loop
--              --  Read the chunk size that is an hex number
--              declare
--  --               L : constant String := Net.Buffered.Get_Line (Sock);
--                 L : constant String := "toto";
--              begin
--                 Size := Stream_Element_Offset
--                   (Utils.Hex_Value (Strings.Fixed.Trim (L, Strings.Both)));
--              end;

--              if Size = 0 then
--                 Skip_Line;
--                 exit;

--              else
--                 if Data_Last + Size > Data'Last then

--                    Tmp := new Stream_Element_Array
--                      (1 ..
--                         Stream_Element_Offset'Max
--                           (Data_Last + Size, 2 * Data'Length));

--                    Tmp (1 .. Data_Last) := Data (1 .. Data_Last);
--                    Utils.Free (Data);
--                    Data := Tmp;
--                 end if;

--  --                 Net.Buffered.Read
--  --                   (Sock, Data (Data_Last + 1 .. Data_Last + Size));

--                 Skip_Line;
--                 Data_Last := Data_Last + Size;
--              end if;

--           end loop;

--           --  Strip the unused bytes

--           declare
--              Copy : constant Stream_Element_Array_Access
--                := new Stream_Element_Array (1 .. Data_Last);
--           begin
--              Copy.all := Data (1 .. Data_Last);
--              Utils.Free (Data);
--              return Copy;
--           end;

--        exception
--           when others =>
--              --  Could have been killed by a timeout.
--              Utils.Free (Data);
--              raise;
--        end Read_Chunk;

--     begin
--        Set_Phase (Connection, Receive);

--  --      Parse_Header
--  --        (Connection, Result, Keep_Alive);

--        if not Get_Body then
--           Disconnect;
--           Set_Phase (Connection, Not_Monitored);
--           return;
--        end if;

--  --       --  Read the message body

--        declare
--           TE     : constant String
--             := Response.Header (Result, Messages.Transfer_Encoding_Token);
--           CT_Len : constant Integer := Response.Content_Length (Result);
--        begin
--           if TE = "chunked" then

--  --        --  A chuncked message is written on the stream as list of data
--  --             --  chunk. Each chunk has the following format:
--  --             --
--  --             --  <N : the chunk size in hexadecimal> CRLF
--  --             --  <N * BYTES : the data> CRLF
--  --             --
--  --             --  The termination chunk is:
--  --             --
--  --             --  0 CRLF
--  --             --  CRLF
--  --             --

--              Response.Set.Message_Body (Result, Read_Chunk);

--           else
--              if CT_Len = Response.Undefined_Length then

--                 declare

--                    package Stream_Element_Table is new GNAT.Table
--                      (Streams.Stream_Element,
--                       Natural,
--                       Table_Low_Bound => 1,
--                       Table_Initial   => 30_000,
--                       Table_Increment => 25);

--                    procedure Add (B : Streams.Stream_Element_Array);
--                    --  Add B to Data

--                    procedure Read_Until_Close;
--                    --  Read data on socket, stop when the socket is closed.

--                    ---------
--                    -- Add --
--                    ---------

--                    procedure Add (B : Streams.Stream_Element_Array) is
--                    begin
--                       for K in B'Range loop
--                          Stream_Element_Table.Append (B (K));
--                       end loop;
--                    end Add;

--                    ----------------------
--                    -- Read_Until_Close --
--                    ----------------------

--                    procedure Read_Until_Close is
--                    begin
--                       loop
--                          declare
--                             Data : constant Streams.Stream_Element_Array
--                               := (45, 56);
--  --                             Data : constant Streams.Stream_Element_Array
--  --                               := Net.Buffered.Read (Sock);
--                          begin
--                             Add (Data);
--                          end;
--                       end loop;
--                    exception
--  --                     when Net.Socket_Error =>
--                       when others =>
--                          null;
--                    end Read_Until_Close;

--                 begin
--                    Read_Until_Close;

--                    Response.Set.Message_Body (Result,
--                      (Streams.Stream_Element_Array
--                         (Stream_Element_Table.Table
--                            (1 .. Stream_Element_Table.Last))));

--                    Stream_Element_Table.Free;
--                 end;

--              else
--                 if CT_Len = 0 then
--                    Response.Set.Message_Body
--                      (Result, Streams.Stream_Element_Array'(1 .. 0 => 0));

--                 else
--                    declare
--                       Elements : constant Stream_Element_Array_Access
--                         := Read_Binary_Message (CT_Len);
--                    begin
--                       Response.Set.Message_Body (Result, Elements);
--                    end;
--                 end if;
--              end if;
--           end if;

--        end;

--        Disconnect;

--        Set_Phase (Connection, Not_Monitored);
--     end Get_Response;

   ----------
   -- Head --
   ----------

   function Head
     (URL        : String;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Head (Connection, Result);
      Close (Connection);

      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Head;

   ----------
   -- Head --
   ----------

   procedure Head
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      URI        : String := No_Data)
   is
      --  this is the main 'head' function. HEAD is like GET, but
      --  without message body in the response

      use AWS.URL;

   begin

      if URI /= No_Data then
         declare
            Overriding_URI : URL.Object := Parse (URI);
            Parameters : constant Unbounded_String :=
              To_Unbounded_String (URL.Query (Overriding_URI));
         begin
            Connection.Host_URL := Overriding_URI;
            Connection.Host := To_Unbounded_String (Host (Overriding_URI));

            Handle_Request (Connection, "HEAD", Parameters, Result);
         end;
      else
         declare
            Parameters : constant Unbounded_String :=
              To_Unbounded_String (URL.Query (Connection.Host_URL));
         begin
            Handle_Request (Connection, "HEAD", Parameters, Result);
         end;
      end if;

   end Head;

   -----------------------------
   -- Open_Send_Common_Header --
   -----------------------------

--     procedure Open_Send_Common_Header
--       (Connection : in out HTTP_Connection;
--        Method     : String;
--        URI        : String)
--     is
--        pragma Warnings (Off);
--        pragma Unreferenced (Method);
--        pragma Warnings (On);
--  --      Sock    : Net.Socket_Access renames Connection.Socket;
--        No_Data : Unbounded_String renames Null_Unbounded_String;

--        procedure Send_Authentication_Header
--          (Token       : String;
--           Data        : in out Authentication_Type);
--        --  Send the authentication header for proxy or for server.

--  --      function HTTP_Prefix (Security : Boolean) return String;
--        --  Returns "http://" or "https://" if Security is set to True.

--  --      function Persistence return String;
--     --  Returns "Keep-Alive" is we have a persistent connection and "Close"
--        --  otherwise.

--  --      function Port_Not_Default (Port : Positive) return String;
--    --  Returns the port image (preceded by character ':') if it is not the
--        --  default port.

--        -----------------
--        -- HTTP_Prefix --
--        -----------------

--  --        function HTTP_Prefix (Security : Boolean) return String is
--  --        begin
--  --           if Security then
--  --              return "https://";
--  --           else
--  --              return "http://";
--  --           end if;
--  --        end HTTP_Prefix;

--        -----------------
--        -- Persistence --
--        -----------------

--  --        function Persistence return String is
--  --        begin
--  --           if Connection.Persistent then
--  --              return "Keep-Alive";
--  --           else
--  --              return "Close";
--  --           end if;
--  --        end Persistence;

--        ----------------------
--        -- Port_Not_Default --
--        ----------------------

--  --        function Port_Not_Default (Port : Positive) return String is
--  --        begin
--  --           if Port = 80 then
--  --              return "";
--  --           else
--  --              declare
--  --                 Port_Image : constant String := Positive'Image (Port);
--  --              begin
--  --                 return ':' & Port_Image (2 .. Port_Image'Last);
--  --              end;
--  --           end if;
--  --        end Port_Not_Default;

--        --------------------------------
--        -- Send_Authentication_Header --
--        --------------------------------

--        procedure Send_Authentication_Header
--          (Token : String;
--           Data  : in out Authentication_Type)
--        is
--           pragma Warnings (Off);
--           pragma Unreferenced (Token);
--           pragma Warnings (On);
--  --         Username : constant String := To_String (Data.User);

--        begin
--           if Data.User /= No_Data
--             and then Data.Pwd /= No_Data
--           then
--              if Data.Work_Mode = Basic then
--                 null;
--  --                 Send_Header
--  --                   (Sock.all,
--  --                    Token & ": Basic "
--  --                      & AWS.Translator.Base64_Encode
--  --                      (Username
--  --                         & ':' & To_String (Data.Pwd)));

--              elsif Data.Work_Mode = Digest then

--                 declare

--  --                  Nonce    : constant String := To_String (Data.Nonce);
--  --                  Realm    : constant String := To_String (Data.Realm);
--  --                  QOP      : constant String := To_String (Data.QOP);

--  --                  function Get_URI return String;
--                    --  Returns the real URI where the request is going to be
--               --  sent. It is either Open_Send_Common_Header.URI parameter
--                --  if it exists (without the HTTP parameters part), or URI
--                    --  part of the Connection.Connect_URL field.

--  --                  function QOP_Data return String;
--                    --  Returns string with qop, cnonce and nc parameters
--                    --  if qop parameter exists in the server auth request,
--                    --  or empty string if not [RFC 2617 - 3.2.2].

--  --                  Response : AWS.Digest.Digest_String;
--  --                  Response : constant AWS.Digest.Digest_String := "t";

--                    -------------
--                    -- Get_URI --
--                    -------------

--  --                    function Get_URI return String is
--  --                       URI_Last : Natural;
--  --                    begin
--  --                       if URI = "" then
--  --                          return URL.Path (Connection.Connect_URL)
--  --                            & URL.File (Connection.Connect_URL);
--  --                       else
--  --                          URI_Last := Strings.Fixed.Index (URI, "?");

--  --                          if URI_Last = 0 then
--  --                             URI_Last := URI'Last;
--  --                          else
--  --                             URI_Last := URI_Last - 1;
--  --                          end if;

--  --                          return URI (URI'First .. URI_Last);
--  --                       end if;
--  --                    end Get_URI;

--  --                  URI : constant String := Get_URI;

--                    --------------
--                    -- QOP_Data --
--                    --------------

--  --                    function QOP_Data return String is
--  --                    CNonce : constant String := AWS.Digest.Create_Nonce;
--  --                    begin
--  --                       if QOP = No_Data then
--  --                          Response := AWS.Digest.Create_Digest
--  --                            (Username => Username,
--  --                             Realm    => Realm,
--  --                             Password => To_String (Data.Pwd),
--  --                             Nonce    => Nonce,
--  --                             Method   => Method,
--  --                             URI      => URI);
--  --                          return "";

--  --                       else
--  --                          Data.NC := Data.NC + 1;

--  --                          declare
--  --                         NC : constant String := Utils.Hex (Data.NC, 8);
--  --                          begin
--  --                             Response := AWS.Digest.Create_Digest
--  --                               (Username => Username,
--  --                                Realm    => Realm,
--  --                                Password => To_String (Data.Pwd),
--  --                                Nonce    => Nonce,
--  --                                CNonce   => CNonce,
--  --                                NC       => NC,
--  --                                QOP      => QOP,
--  --                                Method   => Method,
--  --                                URI      => URI);

--  --                             return "qop=""" & QOP
--  --                               & """, cnonce=""" & CNonce
--  --                               & """, nc=" & NC
--  --                               & ", ";
--  --                          end;
--  --                       end if;
--  --                    end QOP_Data;

--                 begin
--                    null;
--  --                    Send_Header
--  --                      (Sock.all,
--  --                       Token & ": Digest "
--  --                         & QOP_Data
--  --                         & "nonce=""" & Nonce
--  --                         & """, username=""" & Username
--  --                         & """, realm=""" & Realm
--  --                         & """, uri=""" & URI
--  --                         & """, response=""" & Response
--  --                         & """");
--                 end;

--              end if;
--           end if;
--        end Send_Authentication_Header;

--  --        Host_Address : constant String
--  --          := AWS.URL.Host (Connection.Host_URL)
--  --          & Port_Not_Default (AWS.URL.Port (Connection.Host_URL));

--     begin
--        --  Open connection if needed.

--        if not Connection.Opened then
--           Connect (Connection);
--        end if;

--        Set_Phase (Connection, Send);

--        --  Header command.

--        if Connection.Proxy = No_Data then

--           if URI = "" then
--              null;
--  --              Send_Header
--  --                (Sock.all,
--  --                 Method & ' '
--  --          & AWS.URL.Pathname_And_Parameters (Connection.Host_URL, False)
--  --                   & ' ' & HTTP_Version);
--           else
--              --  URI should already be encoded, but to help a bit Windows
--           --  systems who tend to have spaces into URL we encode them here.

--              declare
--                 E_URI : String := URI;
--              begin
--                 for K in E_URI'Range loop
--                    if E_URI (K) = ' ' then
--                       E_URI (K) := '+';
--                    end if;
--                 end loop;

--  --                 Send_Header
--  --                   (Sock.all,
--  --                    Method & ' ' & E_URI & ' ' & HTTP_Version);
--              end;
--           end if;

--  --           Send_Header
--  --             (Sock.all, Messages.Connection (Persistence));

--        else
--           if URI = "" then
--              null;
--  --              Send_Header (Sock.all,
--  --                           Method & ' '
--  --                             & To_String (Connection.Host)
--  --                             & ' ' & HTTP_Version);
--           else
--              null;
--  --              Send_Header
--  --                (Sock.all,
--  --                 Method & ' '
--  --                   & HTTP_Prefix (AWS.URL.Security (Connection.Host_URL))
--  --                   & Host_Address & URI
--  --                   & ' ' & HTTP_Version);
--           end if;

--  --           Send_Header
--  --             (Sock.all, Messages.Proxy_Connection (Persistence));

--        end if;

--        --  Cookie

--        if Connection.Cookie /= No_Data then
--           null;
--  --           Send_Header
--  --             (Sock.all, Messages.Cookie (To_String (Connection.Cookie)));
--        end if;

--  --        Send_Header (Sock.all,
--  --                     Messages.Host (Host_Address));

--  --        Send_Header (Sock.all,
--  --                     Messages.Accept_Type ("text/html, */*"));

--  --        Send_Header (Sock.all,
--  --                     Messages.Accept_Language ("fr, us"));

--  --        Send_Header (Sock.all,
--  --               Messages.User_Agent ("AWS (Ada Web Server) v" & Version));

--        --  User Authentification

--        Send_Authentication_Header
--          (Messages.Authorization_Token, Connection.Auth (WWW));

--        --  Proxy Authentification

--        Send_Authentication_Header
--          (Messages.Proxy_Authorization_Token, Connection.Auth (Proxy));

--        --  SOAP header

--        if Connection.SOAPAction /= No_Data then
--           null;
--  --           Send_Header
--  --             (Sock.all,
--  --              Messages.SOAPAction (To_String (Connection.SOAPAction)));
--        end if;

--        Set_Phase (Connection, Not_Monitored);
--     end Open_Send_Common_Header;

   ------------------
   -- Parse_Header --
   ------------------

--   procedure Parse_Header
--     (Connection        : in out HTTP_Connection;
--      Answer            :    out Response.Data;
--      Keep_Alive        :    out Boolean)
--   is
--      --      Sock : Net.Socket_Type'Class renames Connection.Socket.all;

--       Status : Messages.Status_Code;

--    Request_Auth_Mode : array (Authentication_Level) of Authentication_Mode
--         := (others => Any);

--       procedure Parse_Authenticate_Line
--         (Level     : Authentication_Level;
--          Auth_Line : String);
--     --  Parses Authentication request line and fill Connection.Auth (Level)
--       --  field with the information read on the line. Handle WWW and Proxy
--       --  authentication.

--       procedure Read_Status_Line;
--       --  Read the status line.

--       procedure Set_Keep_Alive (Data : String);
--       --  Set the Parse_Header.Keep_Alive depending on data from the
--       --  Proxy-Connection or Connection header line.

--       function "+" (S : String) return Unbounded_String
--              renames To_Unbounded_String;

--       -----------------------------
--       -- Parse_Authenticate_Line --
--       -----------------------------

--       procedure Parse_Authenticate_Line
--         (Level     : Authentication_Level;
--          Auth_Line : String)
--       is
--          use Ada.Characters.Handling;

--          Basic_Token  : constant String := "BASIC";
--          Digest_Token : constant String := "DIGEST";

--          Auth         : Authentication_Type renames Connection.Auth (Level);

--          Request_Mode : Authentication_Mode;

--          Read_Params  : Boolean := False;
--          --  Set it to true when the authentication
--          --  mode is stronger then before.

--          procedure Value
--            (Item : String;
--             Quit : in out Boolean);
--          --  Routine receiving unnamed value during parsing of
--          --  authentication line.

--          procedure Named_Value
--            (Name  : String;
--             Value : String;
--             Quit  : in out Boolean);
--          --  Routine receiving name/value pairs during parsing of
--          --  authentication line.

--          -----------------
--          -- Named_Value --
--          -----------------

--          procedure Named_Value
--            (Name  : String;
--             Value : String;
--             Quit  : in out Boolean)
--          is
--             pragma Warnings (Off, Quit);
--             U_Name : constant String := To_Upper (Name);
--          begin
--             if not Read_Params then
--                return;
--             end if;

--             if U_Name = "REALM" then
--                Auth.Realm := +Value;

--             elsif U_Name = "NONCE" then
--                Auth.Nonce := +Value;

--             elsif U_Name = "QOP" then
--                Auth.QOP   := +Value;

--             elsif U_Name = "ALGORITHM" then
--                if Value /= "MD5" then
--                   Ada.Exceptions.Raise_Exception
--                     (Constraint_Error'Identity,
--                      "Only MD5 algorithm is supported.");
--                end if;

--             --  The parameter Stale is true when the Digest value is correct
--             --  but the nonce value is too old or incorrect.
--             --
--             --  This mean that an interactive HTTP client should not ask
--          --  name/password from the user, and try to use name/password from
--             --  the previous successful authentication attempt.
--             --  We do not need to check Stale authentication parameter
--           --  for now, because our client is not interactive, so we are not
--           --  going to ask user to input the name/password anyway. We could
--             --  uncomment it later, when we would provide some interactive
--             --  behavior to AWS.Client or interface to the interactive
--             --  programs by callback to the AWS.Client.
--             --
--             --  elsif U_Name = "STALE" then
--             --     null;
--             end if;
--          end Named_Value;

--          -----------
--          -- Value --
--          -----------

--          procedure Value
--            (Item : String;
--             Quit : in out Boolean)
--          is
--             pragma Warnings (Off, Quit);
--             Mode_Image : constant String := To_Upper (Item);
--          begin
--             if Mode_Image = Digest_Token then
--                Request_Mode := Digest;
--             elsif Mode_Image = Basic_Token then
--                Request_Mode := Basic;
--             end if;

--             Read_Params := Request_Mode > Request_Auth_Mode (Level);

--             if Read_Params then
--                Request_Auth_Mode (Level) := Request_Mode;
--                Auth.Requested := True;
--                Auth.Work_Mode := Request_Mode;
--                Auth.NC        := 0;
--             end if;
--          end Value;

--          -----------
--          -- Parse --
--          -----------

--          procedure Parse is new Headers.Values.Parse (Value, Named_Value);

--       begin
--          Parse (Auth_Line);
--       end Parse_Authenticate_Line;

--       -----------------------
--       --  Read_Status_Line --
--       -----------------------

--       procedure Read_Status_Line is

--          function Get_Full_Line return String;
--          --  Returns a full HTTP line (handle continuation line)
--          --
--          --  ??? This is non-standard and as been implemented because some
--          --  Lotus Domino servers do send a Reason-Phrase with continuation
--          --  line. This is clearly not valid see [RFC 2616 - 6.1].

--          -------------------
--          -- Get_Full_Line --
--          -------------------

--          function Get_Full_Line return String is
--  --            Line   : constant String    := Net.Buffered.Get_Line (Sock);
--  --            N_Char : constant Character := Net.Buffered.Peek_Char (Sock);
--             Line : constant String := "toto";
--  --            N_Char : constant Character := 'T';
--          begin
--  --            if N_Char = ' ' or else N_Char = ASCII.HT then
--                --  Next line is a continuation line [RFC 2616 - 2.2], but
--                --  again this is non standard here, see comment above.
--  --               return Line & Get_Full_Line;
--  --            else
--                 return Line;
--  --            end if;
--          end Get_Full_Line;

--          Line : constant String := Get_Full_Line;

--       begin
--          Debug_Message ("< ", Line);

--          --  Checking the first line in the HTTP header.
--          --  It must match Messages.HTTP_Token.

--          if Messages.Match (Line, Messages.HTTP_Token) then
--             Status := Messages.Status_Code'Value
--                  ('S' & Line (Messages.HTTP_Token'Last + 5
--                                 .. Messages.HTTP_Token'Last + 7));
--             Response.Set.Status_Code (Answer, Status);

--             --  By default HTTP/1.0 connection is not keep-alive but
--             --  HTTP/1.1 is keep-alive

--             Keep_Alive
--               := Line (Messages.HTTP_Token'Last + 1
--                          .. Messages.HTTP_Token'Last + 3) >= "1.1";
--          else
--             --  or else it is wrong answer from server.
--             Ada.Exceptions.Raise_Exception (Protocol_Error'Identity, Line);
--          end if;
--       end Read_Status_Line;

--       --------------------
--       -- Set_Keep_Alive --
--       --------------------

--       procedure Set_Keep_Alive (Data : String) is
--       begin
--          if Messages.Match (Data, "Close") then
--             Keep_Alive := False;

--          elsif Messages.Match (Data, "Keep-Alive") then
--             Keep_Alive := True;
--          end if;
--       end Set_Keep_Alive;

--       use type Messages.Status_Code;

--    begin
--       for Level in Authentication_Level'Range loop
--          Connection.Auth (Level).Requested := False;
--       end loop;

--       Read_Status_Line;
--  --      Response.Set.Read_Header (Sock, Answer);

--       --  ??? we should not expect 100 response message after the body sent.
--       --  This code needs to be fixed.
--       --  We should expect 100 status line only before sending the message
--       --  body to server.
--       --  And we should send Expect: header line in the header if we could
--       --  deal with 100 status code.
--       --  See [RFC 2616 - 8.2.3] use of the 100 (Continue) Status.

--       if Status = Messages.S100 then
--          Read_Status_Line;
--  --         Response.Set.Read_Header (Sock, Answer);
--       end if;

--       Set_Keep_Alive (Response.Header (Answer, Messages.Connection_Token));

--       Set_Keep_Alive (Response.Header
--         (Answer, Messages.Proxy_Connection_Token));

--       Connection.Cookie := +Response.Header
--         (Answer, Messages.Set_Cookie_Token);

--       Parse_Authenticate_Line
--         (WWW,
--          Response.Header (Answer, Messages.WWW_Authenticate_Token));

--       Parse_Authenticate_Line
--         (Proxy,
--          Response.Header (Answer, Messages.Proxy_Authenticate_Token));
--    end Parse_Header;

   ----------
   -- Post --
   ----------

   function Post
     (URL        : String;
      Data       : String;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout)
      return Response.Data
   is
      use Streams;
   begin
      return Post (URL, Translator.To_Stream_Element_Array (Data),
                   User, Pwd, Proxy, Proxy_User, Proxy_Pwd, Timeouts);
   end Post;

   ----------
   -- Post --
   ----------

   function Post
     (URL        : String;
      Data       : Streams.Stream_Element_Array;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Post (Connection, Result, Data);
      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Post;

   ----------
   -- Post --
   ----------

   procedure Post
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : Streams.Stream_Element_Array;
      URI        : String := No_Data)
   is

      --  this is the main 'post' function, as all other 'post'
      --  functions call this one

--      No_Data   : Unbounded_String renames Null_Unbounded_String;
--      Try_Count : Natural := Connection.Retry;

--      Auth_Attempts : Auth_Attempts_Count := (others => 2);
--      Auth_Is_Over  : Boolean;

      use AWS.URL;

   begin

      if URI /= No_Data then
         declare
            Overriding_URI : URL.Object := Parse (URI);
         begin
            Connection.Host_URL := Overriding_URI;
            Connection.Host := To_Unbounded_String (Host (Overriding_URI));
         end;
         --  the parameter URI overrides the host set in
         --  Connection. This is not meant for parameters
      end if;

      Handle_Request (Connection, "POST",
                      AWS.Translator.To_Unbounded_String (Data), Result);

--       Retry : loop
--          begin
--             Open_Send_Common_Header (Connection, "POST", URI);

--             declare
--  --              Sock : Net.Socket_Type'Class renames Connection.Socket.all;
--             begin

--                if Connection.SOAPAction = No_Data then
--                   null;
--  --                    Send_Header
--  --                      (Sock,
--  --                       Messages.Content_Type (MIME.Appl_Form_Data));

--                else
--                   null;
--  --                    Send_Header
--  --                      (Sock,
--  --                       Messages.Content_Type (MIME.Text_XML));
--                end if;

--                --  Send message Content_Length

--  --               Send_Header (Sock, Messages.Content_Length (Data'Length));

--  --               Net.Buffered.New_Line (Sock);

--                --  Send message body

--  --               Net.Buffered.Write (Sock, Data);
--             end;

--             --  Get answer from server

--             Get_Response (Connection, Result, not Connection.Server_Push);

--             Decrement_Authentication_Attempt
--               (Connection, Auth_Attempts, Auth_Is_Over);

--             if Auth_Is_Over then
--                return;
--             end if;

--          exception
--             when others => raise;
--  --              when Net.Socket_Error =>

--  --                 Disconnect (Connection);

--  --                 if Try_Count = 0 then
--  --                    Result := Response.Build
--  --                      (MIME.Text_HTML, "Post Timeout", Messages.S408);
--  --                    Set_Phase (Connection, Not_Monitored);
--  --                    exit Retry;
--  --                 end if;

--  --                 Try_Count := Try_Count - 1;
--          end;
--       end loop Retry;
   end Post;

   ----------
   -- Post --
   ----------

   procedure Post
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : String;
      URI        : String := No_Data) is
   begin
      Post (Connection, Result,
            Translator.To_Stream_Element_Array (Data), URI);
   end Post;

   ---------
   -- Put --
   ---------

   function Put
     (URL        : String;
      Data       : String;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Put (Connection, Result, Data);
      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : String;
      URI        : String          := No_Data)
   is
      --  this is the main 'put' function

--        Keep_Alive : Boolean;

      --      Try_Count  : Natural := Connection.Retry;

--      Auth_Attempts : Auth_Attempts_Count := (others => 2);
--      Auth_Is_Over  : Boolean;

      use AWS.URL;

   begin

      if URI /= No_Data then
         declare
            Overriding_URI : URL.Object := Parse (URI);
         begin
            Connection.Host_URL := Overriding_URI;
            Connection.Host := To_Unbounded_String (Host (Overriding_URI));
         end;
         --  the parameter URI overrides the host set in
         --  Connection. This is not meant for parameters
      end if;

      Handle_Request (Connection, "PUT", To_Unbounded_String (Data), Result);

--       Retry : loop

--          begin
--             Open_Send_Common_Header (Connection, "PUT", URI);

--             --  Send message Content_Length

--             --              Send_Header
--         --  (Connection.Socket.all, Messages.Content_Length (Data'Length));

--             --            Net.Buffered.New_Line (Connection.Socket.all);

--             --  Send message body

--          --            Net.Buffered.Put_Line (Connection.Socket.all, Data);

--             --  Get answer from server

--             Parse_Header
--               (Connection, Result, Keep_Alive);

--             if not Keep_Alive then
--                Disconnect (Connection);
--             end if;

--             Decrement_Authentication_Attempt
--               (Connection, Auth_Attempts, Auth_Is_Over);

--             if Auth_Is_Over then
--                return;
--             end if;

--          exception
--             when others => raise;
--             --      when Net.Socket_Error =>

--             --         Disconnect (Connection);

--             --         if Try_Count = 0 then
--             --            Result := Response.Build
--             --              (MIME.Text_HTML, "Put Timeout", Messages.S408);
--             --            Set_Phase (Connection, Not_Monitored);
--             --            exit Retry;
--             --         end if;

--             --         Try_Count := Try_Count - 1;
--          end;
--       end loop Retry;
   end Put;

   ----------------
   -- Read_Until --
   ----------------

   function Read_Until
     (Connection : HTTP_Connection;
      Delimiter  : String)
      return String
   is
      Result     : Unbounded_String;
   begin
      Read_Until (Connection.Self.all, Delimiter, Result);
      return To_String (Result);
   end Read_Until;

   procedure Read_Until
     (Connection : in out HTTP_Connection;
      Delimiter  : String;
      Result     : in out Ada.Strings.Unbounded.Unbounded_String)
   is
      Sample_Idx : Natural := Delimiter'First;
      Buffer     : String (1 .. 1024);

   begin
      Set_Phase (Connection, Receive);

      Main : loop
         for I in Buffer'Range loop
            begin
--               Buffer (I) := Net.Buffered.Get_Char (Connection.Socket.all);
               Buffer (I) := 'T';
            exception
               when others =>
--                 when Net.Socket_Error =>
--                    Append (Result, Buffer (Buffer'First .. I - 1));
                  exit Main;
            end;

            if Buffer (I) = Delimiter (Sample_Idx) then

               if Sample_Idx = Delimiter'Last then
                  Append (Result, Buffer (Buffer'First .. I));
                  exit Main;
               else
                  Sample_Idx := Sample_Idx + 1;
               end if;

            else
               Sample_Idx := Delimiter'First;
            end if;
         end loop;

         Append (Result, Buffer);
      end loop Main;

      Set_Phase (Connection, Not_Monitored);
   end Read_Until;

   -----------------
   -- Send_Header --
   -----------------

--     procedure Send_Header
--       (Sock : Net.Socket_Type'Class;
--        Data : String) is
--     begin
--        Net.Buffered.Put_Line (Sock, Data);
--        Debug_Message ("> ", Data);
--     end Send_Header;

   ------------------------
   -- Set_Authentication --
   ------------------------

   procedure Set_Authentication
     (Auth       :    out Authentication_Type;
      User       : String;
      Pwd        : String;
      Mode       : Authentication_Mode) is
   begin
      Auth.User      := To_Unbounded_String (User);
      Auth.Pwd       := To_Unbounded_String (Pwd);
      Auth.Init_Mode := Mode;

      --  The Digest authentication could not be send without
      --  server authentication request, becouse client have to have nonce
      --  value, so in the Digest and Any authentication modes we are not
      --  setting up Work_Mode to the exact value.
      --  But for Basic authentication we are sending just username/password,
      --  and do not need any information from server for do it.
      --  So if the client want to authenticate "Basic", we are setting up
      --  Work_Mode right now.

      if Mode = Basic then
         Auth.Work_Mode := Basic;
      end if;
   end Set_Authentication;

   ---------------
   -- Set_Debug --
   ---------------

   procedure Set_Debug (On : Boolean) is
   begin
      Debug_On := not Debug_On;
      Debug_On := On;
      AWS.Headers.Set.Debug (On);
   end Set_Debug;

   ---------------
   -- Set_Phase --
   ---------------

   procedure Set_Phase
     (Connection : in out HTTP_Connection;
      Phase      : Client_Phase) is
   begin
      Connection.Current_Phase := Phase;

--      if Connection.Cleaner /= null then
--         Connection.Cleaner.Next_Phase;
--      end if;
   end Set_Phase;

   ------------------------------
   -- Set_Proxy_Authentication --
   ------------------------------

   procedure Set_Proxy_Authentication
     (Connection : in out HTTP_Connection;
      User       : String;
      Pwd        : String;
      Mode       : Authentication_Mode) is
   begin
      Set_Authentication
        (Auth => Connection.Auth (Proxy),
         User => User,
         Pwd  => Pwd,
         Mode => Mode);
   end Set_Proxy_Authentication;

   ----------------------------
   -- Set_WWW_Authentication --
   ----------------------------

   procedure Set_WWW_Authentication
     (Connection : in out HTTP_Connection;
      User       : String;
      Pwd        : String;
      Mode       : Authentication_Mode) is
   begin
      Set_Authentication
        (Auth => Connection.Auth (WWW),
         User => User,
         Pwd  => Pwd,
         Mode => Mode);
   end Set_WWW_Authentication;

   ---------------
   -- SOAP_Post --
   ---------------

   function SOAP_Post
     (URL        : String;
      Data       : String;
      SOAPAction : String;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              SOAPAction => SOAPAction,
              Persistent => False,
              Timeouts   => Timeouts);

      Post (Connection, Result, Data);
      Close (Connection);
      return Result;
   end SOAP_Post;

   function SOAP_Post
     (Connection : access HTTP_Connection;
      Data       : String)
      return Response.Data
   is
      Result     : Response.Data;
   begin
      Post (Connection.all, Result, Data);
      return Result;
   end SOAP_Post;

   function Host (Connection : HTTP_Connection) return Unbounded_String
   is
   begin
      return Connection.Host;
   end Host;

   function Host_URL (Connection : HTTP_Connection) return AWS.URL.Object
   is
   begin
      return Connection.Host_URL;
   end Host_URL;

   ------------
   -- Upload --
   ------------

--     procedure Upload
--       (Connection : in out HTTP_Connection;
--        Result     :    out Response.Data;
--        Filename   : String;
--        URI        : String := No_Data)
--     is
--  --      Pref_Suf  : constant String        := "--";
--  --      Now       : constant Calendar.Time := Calendar.Clock;
--  --      Boundary  : constant String
--  --        := "AWS_File_Upload-" & GNAT.Calendar.Time_IO.Image (Now, "%s");

--  --        CT        : constant String
--  --          := Messages.Content_Type (MIME.Content_Type (Filename));

--  --        CD        : constant String
--  --    := Messages.Content_Disposition ("form-data", "filename", Filename);

--  --      Try_Count :  Natural := Connection.Retry;

--        Auth_Attempts : Auth_Attempts_Count := (others => 2);
--        Auth_Is_Over  : Boolean;

--  --      function Content_Length return Integer;
--        --  Returns the total message content length.

--        procedure Send_File;
--        --  Send file content to the server.

--        --------------------
--        -- Content_Length --
--        --------------------

--  --        function Content_Length return Integer is
--  --        begin
--  --           return 2 * Boundary'Length                -- 2 boundaries
--  --     + 2                                     -- second one end with "--"
--  --           + 10                                    -- 5 lines with CR+LF
--  --        + CT'Length                             -- content length header
--  --     + CD'Length                             -- content disposition head
--  --             + Integer (OS_Lib.File_Size (Filename)) -- file size
--  --       + 2;                                    -- CR+LF after file data
--  --        end Content_Length;

--        ---------------
--        -- Send_File --
--        ---------------

--        procedure Send_File is
--  --         Sock     : Net.Socket_Type'Class renames Connection.Socket.all;
--           Buffer   : Streams.Stream_Element_Array (1 .. 4_096);
--           Last     : Streams.Stream_Element_Offset;
--           File     : Streams.Stream_IO.File_Type;
--        begin
--           --  Send multipart message start boundary

--  --         Net.Buffered.Put_Line (Sock, Pref_Suf & Boundary);

--           --  Send Content-Disposition header

--  --         Net.Buffered.Put_Line (Sock, CD);

--           --  Send Content-Type: header

--  --         Net.Buffered.Put_Line (Sock, CT);

--  --         Net.Buffered.New_Line (Sock);

--           --  Send file content

--        Streams.Stream_IO.Open (File, Streams.Stream_IO.In_File, Filename);

--           while not Streams.Stream_IO.End_Of_File (File) loop
--              Streams.Stream_IO.Read (File, Buffer, Last);
--  --            Net.Buffered.Write (Sock, Buffer (1 .. Last));
--           end loop;

--           Streams.Stream_IO.Close (File);

--  --         Net.Buffered.New_Line (Sock);

--           --  Send multipart message end boundary

--  --         Net.Buffered.Put_Line (Sock, Pref_Suf & Boundary & Pref_Suf);

--  --      exception
--  --         when others =>
--  --           when Net.Socket_Error =>
--  --              --  Properly close the file if needed
--  --              if Streams.Stream_IO.Is_Open (File) then
--  --                 Streams.Stream_IO.Close (File);
--  --              end if;
--  --            raise;
--        end Send_File;

--     begin
--        Retry : loop
--           begin
--              Open_Send_Common_Header (Connection, "POST", URI);

--              declare
--  --            Sock : Net.Socket_Type'Class renames Connection.Socket.all;
--              begin
--                 --  Send message Content-Type (Multipart/form-data)

--  --                 Send_Header
--  --                   (Sock,
--  --            Messages.Content_Type (MIME.Multipart_Form_Data, Boundary));

--                 --  Send message Content-Length

--  --           Send_Header (Sock, Messages.Content_Length (Content_Length));

--  --               Net.Buffered.New_Line (Sock);

--                 --  Send message body

--                 Send_File;
--              end;

--              --  Get answer from server

--              Get_Response (Connection, Result, not Connection.Server_Push);

--              Decrement_Authentication_Attempt
--                (Connection, Auth_Attempts, Auth_Is_Over);

--              if Auth_Is_Over then
--                 return;
--              end if;

--  --         exception

--  --            when others => raise;
--  --              when Net.Socket_Error =>

--  --                 Disconnect (Connection);

--  --                 if Try_Count = 0 then
--  --                    Result := Response.Build
--  --                      (MIME.Text_HTML, "Upload Timeout", Messages.S408);
--  --                    Set_Phase (Connection, Not_Monitored);
--  --                    exit Retry;
--  --                 end if;

--  --                 Try_Count := Try_Count - 1;
--           end;
--        end loop Retry;
--     end Upload;

--     function Upload
--       (URL        : String;
--        Filename   : String;
--        User       : String          := No_Data;
--        Pwd        : String          := No_Data;
--        Proxy      : String          := No_Data;
--        Proxy_User : String          := No_Data;
--        Proxy_Pwd  : String          := No_Data;
--        Timeouts   : Timeouts_Values := No_Timeout)
--        return Response.Data
--     is
--        Connection : HTTP_Connection;
--        Result     : Response.Data;

--     begin
--        Create (Connection,
--                URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
--                Persistent => False,
--                Timeouts   => Timeouts);

--        Upload (Connection, Result, Filename);

--        Close (Connection);
--        return Result;

--     exception
--        when others =>
--           Close (Connection);
--           raise;
--     end Upload;

end AWS.Client;
