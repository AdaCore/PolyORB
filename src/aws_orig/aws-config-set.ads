------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A W S . C O N F I G . S E T                        --
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

--  This package can be used to Set any AWS parameters.

package AWS.Config.Set is

   ------------------------
   -- Per Server Options --
   ------------------------

   procedure Server_Name (O : in out Object; Value : String);
   --  This is the name of the server as set by AWS.Server.Start.

   procedure WWW_Root (O : in out Object; Value : String);
   --  This is the root directory name for the server. This variable is not
   --  used internally by AWS. It is supposed to be used by the callback
   --  procedures who want to retreive physical objects (images, Web
   --  pages...). The default value is the current working directory.

   procedure Admin_URI (O : in out Object; Value : String);
   --  This is the name of the admin server page as set by AWS.Server.Start.

   procedure Server_Host (O : in out Object; Value : String);
   --  This is the server host as set by the HTTP object declaration.

   procedure Server_Port (O : in out Object; Value : Positive);
   --  This is the server port as set by the HTTP object declaration.

   procedure Security (O : in out Object; Value : Boolean);
   --  Enable security (HTTPS/SSL) if Value is True.

   procedure Certificate (Filename : String);
   --  Set the certificate to be used with the secure server.

   procedure Hotplug_Port (O : in out Object; Value : Positive);
   --  This is the hotplug communication port needed to register and
   --  un-register an hotplug module.

   procedure Max_Connection (O : in out Object; Value : Positive);
   --  This is the max simultaneous connections as set by the HTTP object
   --  declaration.

   procedure Accept_Queue_Size (O : in out Object; Value : Positive);
   --  This is the size of the queue for the incoming requests. Higher this
   --  value will be and less "connection refused" will be reported to the
   --  client.

   procedure Log_File_Directory (O : in out Object; Value : String);
   --  This point to the directory where log files will be written. The
   --  directory returned will end with a directory separator.

   procedure Log_Filename_Prefix (O : in out Object; Value : String);
   --  This is the prefix to use for the log filename.

   procedure Log_Split_Mode (O : in out Object; Value : String);
   --  This is split mode for the log file. Possible values are : Each_Run,
   --  Daily, Monthly and None. Any other values will raise an exception.

   procedure Error_Log_Filename_Prefix (O : in out Object; Value : String);
   --  This is the prefix to use for the log filename.

   procedure Error_Log_Split_Mode (O : in out Object; Value : String);
   --  This is split mode for the log file. Possible values are : Each_Run,
   --  Daily, Monthly and None. Any other values will raise an exception.

   procedure Upload_Directory (O : in out Object; Value : String);
   --  This point to the directory where uploaded files will be stored. The
   --  directory returned will end with a directory separator.

   procedure Session (O : in out Object; Value : Boolean);
   --  Enable session handling is Value is True.

   procedure Cleaner_Wait_For_Client_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for a client request.
   --  This is a timeout for regular cleaning task.

   procedure Cleaner_Client_Header_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for client header.
   --  This is a timeout for regular cleaning task.

   procedure Cleaner_Client_Data_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for client message body.
   --  This is a timeout for regular cleaning task.

   procedure Cleaner_Server_Response_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for client to accept answer.
   --  This is a timeout for regular cleaning task.

   procedure Force_Wait_For_Client_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for a client request.
   --  This is a timeout for urgent request when resources are missing.

   procedure Force_Client_Header_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for client header.
   --  This is a timeout for urgent request when resources are missing.

   procedure Force_Client_Data_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for client message body.
   --  This is a timeout for urgent request when resources are missing.

   procedure Force_Server_Response_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for client to accept answer.
   --  This is a timeout for urgent request when resources are missing.

   procedure Send_Timeout
     (O  : in out Object;
      Value : Duration);
   --  Number of seconds to timeout when sending chunck of data.

   procedure Receive_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timeout when receiving chunck of data.

   procedure Status_Page
     (O : in out Object;
      Value     : String);
   --  Filename for the status page.

   procedure Up_Image (O : in out Object; Value : String);
   --  Filename for the up arrow image used in the status page.

   procedure Down_Image (O : in out Object; Value : String);
   --  Filename for the down arrow image used in the status page.

   procedure Logo_Image (O : in out Object; Value : String);
   --  Filename for the AWS logo image used in the status page.

   procedure Case_Sensitive_Parameters (O : in out Object; Value : Boolean);
   --  Parameters are handled with the case if Value is True.

   procedure Line_Stack_Size (O : in out Object; Value : Positive);
   --  HTTP lines stack size.

   -------------------------
   -- Per Process Options --
   -------------------------

   procedure Session_Cleanup_Interval (Value : Duration);
   --  Number of seconds between each run of the cleaner task to remove
   --  obsolete session data.

   procedure Session_Lifetime (Value : Duration);
   --  Number of seconds to keep a session if not used. After this period the
   --  session data is obsoleted and will be removed during new cleanup.

   procedure Parameter
     (Config        : in out Object;
      Name          : String;
      Value         : String;
      Error_Context : String := "");
   --  Set one of the AWS HTTP per server parameters. Raises Constraint_Error
   --  in case of wrong parameter name or wrong parameter value.
   --  Error_Context may contain additional information about the parameter.
   --  This  message will be added to the Constraint_Error exception.
   --  One way to use Error_Context is to set it with information about
   --  where this parameter come form.

   procedure Parameter
     (Name          : String;
      Value         : String;
      Error_Context : String := "");
   --  Set one of the AWS HTTP per process parameters. See description above.

end AWS.Config.Set;
