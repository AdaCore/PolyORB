------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           A W S . S E R V E R                            --
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

with Ada.Calendar;
with Ada.Exceptions;

with AWS.Config;
with AWS.Default;
with AWS.Dispatchers;
with AWS.Exceptions;
with AWS.Hotplug;
with AWS.Log;
with AWS.Response;
with AWS.Utils;

with PolyORB.Servants;
with PolyORB.References;

package AWS.Server is

   type HTTP is abstract new PolyORB.Servants.Servant with private;
   --  A Web server is an abstract servant

   procedure Run;
   --  Runs PolyORB

   procedure Initialization;
   --  Initializes PolyORB

   ---------------------------
   -- Server initialization --
   ---------------------------

   procedure Start
     (The_Server : in out HTTP'Class;
      Callback   : Response.Callback;
      Config     : AWS.Config.Object);
   --  Start server using a full configuration object. With this routine it is
   --  possible to control all features of the server. A simplified version of
   --  Start is also provided below with the most common options.
   --  User_Config_Filename is a specific configuration file that will parsed
   --  after 'aws.ini', 'prognam.ini', '<servername>.ini' files.

   procedure Start
     (The_Server : in out HTTP'Class;
      Dispatcher : Dispatchers.Handler'Class;
      Config     : AWS.Config.Object);
   --  Idem, but using the dispatcher tagged type instead of callback. See
   --  AWS.Services.Dispatchers and AWS.Dispatchers hierarchies for built-in
   --  services and interface to build your own dispatcher models.
   --  Note that a copy of the Dispatcher is keept into Web_Server. Any
   --  changes done to the Dispatcher object will not be part of the Web
   --  server dispatcher.

   procedure Start
     (The_Server                : in out HTTP'Class;
      Name                      : String;
      Callback                  : Response.Callback;
      Max_Connection            : Positive  := Default.Max_Connection;
      Admin_URI                 : String    := Default.Admin_URI;
      Port                      : Positive  := Default.Server_Port;
      Security                  : Boolean   := False;
      Session                   : Boolean   := False;
      Case_Sensitive_Parameters : Boolean   := True;
      Upload_Directory          : String    := Default.Upload_Directory;
      Line_Stack_Size           : Positive  := Default.Line_Stack_Size);
   --  Start the Web server. Max_Connection is the number of simultaneous
   --  connections the server's will handle (the number of slots in AWS).
   --  Name is just a string used to identify the server. This is used
   --  for example in the administrative page. Admin_URI must be set to enable
   --  the administrative status page. Callback is the procedure to call for
   --  each resource requested. Port is the Web server port. If Security is
   --  set to True the server will use an HTTPS/SSL connection. If Session is
   --  set to True the server will be able to get a status for each client
   --  connected. A session ID is used for that, on the client side it is a
   --  cookie. Case_Sensitive_Parameters if set to False it means that the CGI
   --  parameters name will be handled without case sensitivity. Upload
   --  directory point to a directory where uploaded files will be stored.

   ------------------------
   -- Server termination --
   ------------------------

   procedure Shutdown (The_Server : in out HTTP'Class);
   --  Stop the server and release all associated memory. This routine can
   --  take some time to terminate because it waits for all tasks to terminate
   --  properly before releasing the memory. The log facilities will be
   --  automatically stopped by calling Stop_Log below.

   type Termination is (No_Server, Q_Key_Pressed, Forever);

   procedure Wait (Mode : Termination := No_Server);
   --  The purpose of this procedure is to control the main procedure
   --  termination. This procedure will return only when no server are running
   --  (No_Server mode) or the 'q' key has been pressed. If mode is set to
   --  Forever, Wait will never return and the process will have to be killed.

   --------------------------
   -- Server configuration --
   --------------------------

   function Config (The_Server : HTTP'Class) return AWS.Config.Object;
   --  Returns configuration object for The_Server.

   procedure Set_Unexpected_Exception_Handler
     (The_Server : in out HTTP'Class;
      Handler    : Exceptions.Unexpected_Exception_Handler);
   --  Set the unexpected exception handler. It is called whenever an
   --  unrecoverable error has been detected. The default handler just display
   --  (on standard output) an error message with the location of the
   --  error. By changing this handler it is possible to log or display full
   --  symbolic stack backtrace if needed.

   procedure Set
     (The_Server : in out HTTP'Class;
      Dispatcher : Dispatchers.Handler'Class);
   --  Dynamically associate a new dispatcher object to the server. With the
   --  feature it is possible to change server behavior at runtime. The
   --  complete set of callback procedures will be changed when calling this
   --  routine.

   procedure Set_Security (Certificate_Filename : String);
   --  Set security option for AWS. Certificate_Filename is the name of a file
   --  containing a certificate and the private key. This must be called
   --  before starting the first secure server. After that the call will have
   --  no effect.

   function Get_Server_Reference
     (The_Server : HTTP'Class)
     return PolyORB.References.Ref;
   --  returns the reference to the server. So the application can
   --  then convert it into an IOR, URI or whatever. This should be
   --  called for a running server (i.e. after the Start procedure)

   -----------------
   -- Server Logs --
   -----------------

   procedure Start_Log
     (The_Server      : in out HTTP'Class;
      Split_Mode      : Log.Split_Mode := Log.None;
      Filename_Prefix : String         := "");
   --  Activate server's logging activity. See AWS.Log.

   procedure Stop_Log (The_Server : in out HTTP'Class);
   --  Stop server's logging activity. See AWS.Log.

   procedure Start_Error_Log
     (The_Server      : in out HTTP'Class;
      Split_Mode      : Log.Split_Mode := Log.None;
      Filename_Prefix : String         := "");
   --  Activate server's logging activity. See AWS.Log.

   procedure Stop_Error_Log (The_Server : in out HTTP'Class);
   --  Stop server's logging activity. See AWS.Log.

   type HTTP_Access is access all HTTP;

private

   procedure Default_Unexpected_Exception_Handler
     (E      : Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : Exceptions.Data;
      Answer : in out Response.Data);
   --  Default unexpected exception handler.

   package CNF renames AWS.Config;

   ----------
   -- HTTP --
   ----------

   type HTTP is abstract new PolyORB.Servants.Servant with record
      Self              : HTTP_Access := HTTP'Unchecked_Access;
      --  Point to the record.

      Reference         : PolyORB.References.Ref;
      --  the reference to the servant

      Start_Time        : Ada.Calendar.Time;
      --  Date and Time when server was started.

      Shutdown          : Boolean := True;
      --  True when server is shutdown. This will be set to False when server
      --  will be started.

      Properties        : CNF.Object := CNF.Get_Current;
      --  All server properties controled by the configuration file.

      Log               : AWS.Log.Object;
      --  Loggin support.

      Error_Log         : aliased AWS.Log.Object;
      --  Error loggin support.

      Dispatcher        : Dispatchers.Handler_Class_Access;
      --  Dispatcher for the user actions.

      Dispatcher_Sem    : Utils.RW_Semaphore (Writers => 1);
      --  RW semaphore to be able to change dynamically the Dispatcher object.

      Filters           : Hotplug.Filter_Set;
      --  Hotplug filters are recorded here.

      --  Lines             : Line_Set_Access;
      --  The tasks doing the job.

      --  Slots             : Slots_Access;
      --  Information about each tasks above. This is a protected object to
      --  support concurrency.

      Exception_Handler : Exceptions.Unexpected_Exception_Handler
         := Default_Unexpected_Exception_Handler'Access;
      --  Exception handle used for unexpected errors found on the server
      --  implementation.
   end record;

end AWS.Server;
