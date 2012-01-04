------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           A W S . C O N F I G                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  This package provide an easy way to handle server configuration options.
--
--  If initialization of this package is not done all functions below will
--  return the default value as declared in AWS.Default.

with Ada.Strings.Unbounded;

with AWS.Default;

package AWS.Config is

   use Ada.Strings.Unbounded;

   type Object is private;

   Default_Config : constant Object;

   function Get_Current return Object;
   --  Returns a configuration record. This is the properties as read in files
   --  'aws.ini' and 'progname.ini'. This configuration object holds only the
   --  per-server options.

   ------------------------
   -- Per Server options --
   ------------------------

   function Server_Name (O : Object) return String;
   pragma Inline (Server_Name);
   --  This is the name of the server as set by AWS.Server.Start.

   function WWW_Root (O : Object) return String;
   pragma Inline (WWW_Root);
   --  This is the root directory name for the server. This variable is not
   --  used internally by AWS. It is supposed to be used by the callback
   --  procedures who want to retrieve physical objects (images, Web
   --  pages...). The default value is the current working directory.

   function Admin_URI (O : Object) return String;
   pragma Inline (Admin_URI);
   --  This is the name of the admin server page as set by AWS.Server.Start.

   function Server_Host (O : Object) return String;
   pragma Inline (Server_Host);
   --  This is the server host. Can be used if the computer has a more than
   --  one IP addresses. It is possible to have two servers at the same port
   --  on the same machine, both being binded on different IP addresses.

   function Server_Port (O : Object) return Positive;
   pragma Inline (Server_Port);
   --  This is the server port as set by the HTTP object declaration.

   function Hotplug_Port (O : Object) return Positive;
   pragma Inline (Hotplug_Port);
   --  This is the hotplug communication port needed to register and
   --  un-register an hotplug module.

   function Max_Connection (O : Object) return Positive;
   pragma Inline (Max_Connection);
   --  This is the max simultaneous connections as set by the HTTP object
   --  declaration.

   function Accept_Queue_Size (O : Object) return Positive;
   pragma Inline (Accept_Queue_Size);
   --  This is the size of the queue for the incoming requests. Higher this
   --  value will be and less "connection refused" will be reported to the
   --  client.

   function Log_File_Directory (O : Object) return String;
   pragma Inline (Log_File_Directory);
   --  This point to the directory where log files will be written. The
   --  directory returned will end with a directory separator.

   function Log_Filename_Prefix (O : Object) return String;
   pragma Inline (Log_Filename_Prefix);
   --  This is the prefix to use for the log filename.

   function Log_Split_Mode (O : Object) return String;
   pragma Inline (Log_Split_Mode);
   --  This is split mode for the log file. Possible values are : Each_Run,
   --  Daily, Monthly and None. Any other values will raise an exception.

   function Error_Log_Filename_Prefix (O : Object) return String;
   pragma Inline (Error_Log_Filename_Prefix);
   --  This is the prefix to use for the log filename.

   function Error_Log_Split_Mode (O : Object) return String;
   pragma Inline (Error_Log_Split_Mode);
   --  This is split mode for the log file. Possible values are : Each_Run,
   --  Daily, Monthly and None. Any other values will raise an exception.

   function Upload_Directory (O : Object) return String;
   pragma Inline (Upload_Directory);
   --  This point to the directory where uploaded files will be stored. The
   --  directory returned will end with a directory separator.

   function Session (O : Object) return Boolean;
   pragma Inline (Session);
   --  Returns True if the server session is activated.

   function Cleaner_Wait_For_Client_Timeout (O : Object) return Duration;
   pragma Inline (Cleaner_Wait_For_Client_Timeout);
   --  Number of seconds to timout on waiting for a client request.
   --  This is a timeout for regular cleaning task.

   function Cleaner_Client_Header_Timeout (O : Object) return Duration;
   pragma Inline (Cleaner_Client_Header_Timeout);
   --  Number of seconds to timout on waiting for client header.
   --  This is a timeout for regular cleaning task.

   function Cleaner_Client_Data_Timeout (O : Object) return Duration;
   pragma Inline (Cleaner_Client_Data_Timeout);
   --  Number of seconds to timout on waiting for client message body.
   --  This is a timeout for regular cleaning task.

   function Cleaner_Server_Response_Timeout (O : Object) return Duration;
   pragma Inline (Cleaner_Server_Response_Timeout);
   --  Number of seconds to timout on waiting for client to accept answer.
   --  This is a timeout for regular cleaning task.

   function Force_Wait_For_Client_Timeout (O : Object) return Duration;
   pragma Inline (Force_Wait_For_Client_Timeout);
   --  Number of seconds to timout on waiting for a client request.
   --  This is a timeout for urgent request when resources are missing.

   function Force_Client_Header_Timeout (O : Object) return Duration;
   pragma Inline (Force_Client_Header_Timeout);
   --  Number of seconds to timout on waiting for client header.
   --  This is a timeout for urgent request when resources are missing.

   function Force_Client_Data_Timeout (O : Object) return Duration;
   pragma Inline (Force_Client_Data_Timeout);
   --  Number of seconds to timout on waiting for client message body.
   --  This is a timeout for urgent request when resources are missing.

   function Force_Server_Response_Timeout (O : Object) return Duration;
   pragma Inline (Force_Server_Response_Timeout);
   --  Number of seconds to timout on waiting for client to accept answer.
   --  This is a timeout for urgent request when resources are missing.

   function Send_Timeout (O : Object) return Duration;
   pragma Inline (Send_Timeout);
   --  Number of seconds to timeout when sending chunck of data.

   function Receive_Timeout (O : Object) return Duration;
   pragma Inline (Receive_Timeout);
   --  Number of seconds to timeout when receiving chunck of data.

   function Status_Page (O : Object) return String;
   pragma Inline (Status_Page);
   --  Filename for the status page.

   function Up_Image (O : Object) return String;
   pragma Inline (Up_Image);
   --  Filename for the up arrow image used in the status page.

   function Down_Image (O : Object) return String;
   pragma Inline (Down_Image);
   --  Filename for the down arrow image used in the status page.

   function Logo_Image (O : Object) return String;
   pragma Inline (Logo_Image);
   --  Filename for the AWS logo image used in the status page.

   function Security (O : Object) return Boolean;
   pragma Inline (Security);
   --  Is the server working through th SSL

   function Certificate return String;
   pragma Inline (Certificate);
   --  Returns the certificate to be used with the secure server. Returns the
   --  empty string if the server is not a secure one.

   function Case_Sensitive_Parameters (O : Object) return Boolean;
   pragma Inline (Case_Sensitive_Parameters);
   --  HTTP parameters are case sensitive.

   function Check_URL_Validity (O : Object) return Boolean;
   pragma Inline (Check_URL_Validity);
   --  Server have to check URI for validity. For example it checks that an
   --  URL does not reference a resource above the Web root.

   function Line_Stack_Size (O : Object) return Positive;
   pragma Inline (Line_Stack_Size);
   --  HTTP lines stack size.

   -------------------------
   -- Per Process options --
   -------------------------

   function Session_Cleanup_Interval return Duration;
   pragma Inline (Session_Cleanup_Interval);
   --  Number of seconds between each run of the cleaner task to remove
   --  obsolete session data.

   function Session_Lifetime return Duration;
   pragma Inline (Session_Lifetime);
   --  Number of seconds to keep a session if not used. After this period the
   --  session data is obsoleted and will be removed during new cleanup.

private

   --  List of token (keyword) recognized by the parser. There must be one
   --  entry for every option name to be handled.

   type Parameter_Name is
      --  Per server option
     (Server_Name,
      WWW_Root,
      Admin_URI,
      Server_Host,
      Server_Port,
      Security,
      Hotplug_Port,
      Max_Connection,
      Accept_Queue_Size,
      Log_File_Directory,
      Log_Filename_Prefix,
      Log_Split_Mode,
      Error_Log_Filename_Prefix,
      Error_Log_Split_Mode,
      Upload_Directory,
      Session,
      Cleaner_Wait_For_Client_Timeout,
      Cleaner_Client_Header_Timeout,
      Cleaner_Client_Data_Timeout,
      Cleaner_Server_Response_Timeout,
      Force_Wait_For_Client_Timeout,
      Force_Client_Header_Timeout,
      Force_Client_Data_Timeout,
      Force_Server_Response_Timeout,
      Send_Timeout,
      Receive_Timeout,
      Status_Page,
      Up_Image,
      Down_Image,
      Logo_Image,
      Line_Stack_Size,
      Check_URL_Validity,
      Case_Sensitive_Parameters,
      --  Per process options
      Session_Cleanup_Interval,
      Certificate,
      Session_Lifetime);

   subtype Server_Parameter_Name is Parameter_Name
     range Server_Name .. Case_Sensitive_Parameters;

   subtype Process_Parameter_Name is Parameter_Name
     range Session_Cleanup_Interval .. Session_Lifetime;

   type Value_Type  is (Str, Dir, Pos, Dur, Bool);

   type Values (Kind : Value_Type := Str) is record

      case Kind is
         when Str =>
            Str_Value : Unbounded_String;

         when Dir =>
            Dir_Value : Unbounded_String;

         when Pos =>
            Pos_Value : Positive;

         when Dur =>
            Dur_Value : Duration;

         when Bool =>
            Bool_Value : Boolean;

      end case;
   end record;

   type Parameter_Set is array (Parameter_Name range <>) of Values;

   Default_Parameters : constant Parameter_Set (Server_Parameter_Name)
     := (Cleaner_Wait_For_Client_Timeout =>
           (Dur, Default.Cleaner_Wait_For_Client_Timeout),

         Cleaner_Client_Header_Timeout =>
           (Dur, Default.Cleaner_Client_Header_Timeout),

         Cleaner_Client_Data_Timeout =>
           (Dur, Default.Cleaner_Client_Data_Timeout),

         Cleaner_Server_Response_Timeout =>
           (Dur, Default.Cleaner_Server_Response_Timeout),

         Force_Wait_For_Client_Timeout =>
           (Dur, Default.Force_Wait_For_Client_Timeout),

         Force_Client_Header_Timeout =>
           (Dur, Default.Force_Client_Header_Timeout),

         Force_Client_Data_Timeout =>
           (Dur, Default.Force_Client_Data_Timeout),

         Force_Server_Response_Timeout =>
           (Dur, Default.Force_Server_Response_Timeout),

         Send_Timeout =>
           (Dur, Default.Send_Timeout),

         Receive_Timeout =>
           (Dur, Default.Receive_Timeout),

         Status_Page =>
           (Str, To_Unbounded_String (Default.Status_Page)),

         Up_Image =>
           (Str, To_Unbounded_String (Default.Up_Image)),

         Down_Image =>
           (Str, To_Unbounded_String (Default.Down_Image)),

         Logo_Image =>
           (Str, To_Unbounded_String (Default.Logo_Image)),

         Admin_URI =>
           (Str, To_Unbounded_String (Default.Admin_URI)),

         Server_Name =>
           (Str, To_Unbounded_String (Default.Server_Name)),

         WWW_Root =>
           (Dir, To_Unbounded_String (Default.WWW_Root)),

         Log_File_Directory =>
           (Dir, To_Unbounded_String (Default.Log_File_Directory)),

         Log_Filename_Prefix =>
           (Str, To_Unbounded_String (Default.Log_Filename_Prefix)),

         Log_Split_Mode =>
           (Str, To_Unbounded_String (Default.Log_Split_Mode)),

         Error_Log_Filename_Prefix =>
           (Str, To_Unbounded_String (Default.Error_Log_Filename_Prefix)),

         Error_Log_Split_Mode =>
           (Str, To_Unbounded_String (Default.Error_Log_Split_Mode)),

         Upload_Directory =>
           (Dir, To_Unbounded_String (Default.Upload_Directory)),

         Max_Connection =>
           (Pos, Default.Max_Connection),

         Accept_Queue_Size =>
           (Pos, Default.Accept_Queue_Size),

         Server_Host =>
           (Str, Null_Unbounded_String),

         Server_Port =>
           (Pos, Default.Server_Port),

         Hotplug_Port =>
           (Pos, Default.Hotplug_Port),

         Session =>
           (Bool, Default.Session),

         Security =>
           (Bool, Default.Security),

         Case_Sensitive_Parameters =>
           (Bool, Default.Case_Sensitive_Parameters),

         Check_URL_Validity =>
           (Bool, Default.Check_URL_Validity),

         Line_Stack_Size =>
           (Pos, Default.Line_Stack_Size));

   type Object is record
      P : Parameter_Set (Server_Parameter_Name) := Default_Parameters;
   end record;

   Default_Config : constant Object
     := (P => Default_Parameters);

   Server_Config : Object;
   --  This variable will be updated with options found in 'aws.ini' and
   --  'progname.ini'.

   Process_Options : Parameter_Set (Process_Parameter_Name)
     := (Session_Cleanup_Interval =>
           (Dur, Default.Session_Cleanup_Interval),

         Certificate =>
           (Str, To_Unbounded_String (Default.Certificate)),

         Session_Lifetime =>
           (Dur, Default.Session_Lifetime));

end AWS.Config;
