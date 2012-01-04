------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              A W S . L O G                               --
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

--  This package handle the logging facility for AWS. The log file is named
--  '<progname>-Y-M-D.log' and is written by default in the directory where
--  the server is launched, see configuration file.
--
--  Note that this package is used internaly by AWS to log server requests but
--  it can also be used by users to handle application's log.

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with AWS.Status;
with AWS.Response;
with AWS.Messages;

package AWS.Log is

   type Object is limited private;
   --  A log object. It must be activated by calling Start below.

   type Split_Mode is (None, Each_Run, Daily, Monthly);
   --  It specifies when to create a new log file.
   --  None     : all log info gets accumulated into the same file.
   --  Each_Run : a new log file is created each time the server is started.
   --  Daily    : a new log file is created each day.
   --  Monthly  : a new log file is created each month.

   Not_Specified : constant String := "";

   procedure Start
     (Log             : in out Object;
      Split           : Split_Mode := None;
      File_Directory  : String     := Not_Specified;
      Filename_Prefix : String     := Not_Specified);
   --  Activate server's activity logging. Split indicate the way the log file
   --  should be created. Log_File_Prefix is the log filename prefix. If it is
   --  not specified the default prefix is the program name.

   procedure Write
     (Log          : in out Object;
      Connect_Stat : Status.Data;
      Answer       : Response.Data);
   --  Write log info if activated (i.e. Start routine above has been called).

   procedure Write
     (Log            : in out Object;
      Connect_Stat   : Status.Data;
      Status_Code    : Messages.Status_Code;
      Content_Length : Natural);
   --  Write log info if activated (i.e. Start routine above has been called).
   --  This version separated the Content_Length from Status.Data, this is
   --  required for example in the case of a user defined stream content. See
   --  AWS.Resources.Stream.

   procedure Write
     (Log          : in out Object;
      Connect_Stat : Status.Data;
      Data         : String);
   --  Write user's log info if activated.  (i.e. Start routine above has been
   --  called).

   procedure Write
     (Log  : in out Object;
      Data : String);
   --  Write Data into the log file. This Data is unstructured, only a time
   --  tag prefix is prepended to Data. This routine is designed to be used
   --  for user's info in error log file.

   procedure Stop (Log : in out Object);
   --  Stop logging activity.

   function Is_Active (Log : Object) return Boolean;
   --  Returns True if Log is activated.

   function Filename (Log : Object) return String;
   --  Returns current log filename or the empty string if the log is not
   --  activated.

   function Mode (Log : Object) return Split_Mode;
   --  Returns the split mode. None will be returned if log is not activated.

private

   use Ada;
   use Ada.Strings.Unbounded;

   type Object is limited record
      File            : Text_IO.File_Type;
      File_Directory  : Unbounded_String;
      Filename_Prefix : Unbounded_String;
      Split           : Split_Mode := None;
      Current_Tag     : Positive;
   end record;

end AWS.Log;
