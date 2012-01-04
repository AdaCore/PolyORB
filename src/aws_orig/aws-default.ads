------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          A W S . D E F A U L T                           --
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

--  This package contains the default AWS configuration values. These values
--  are used to initialize the configuration objects. Users should not modify
--  the values here, see AWS.Config.* API.

package AWS.Default is

   pragma Pure;

   Server_Name               : constant String := "AWS Module";
   WWW_Root                  : constant String := "./";
   Admin_URI                 : constant String := "";
   Server_Port               : constant        := 8080;
   Hotplug_Port              : constant        := 8888;
   Max_Connection            : constant        := 5;
   Accept_Queue_Size         : constant        := 64;
   Upload_Directory          : constant String := "./";

   --  Log values. The character '@' in the error log filename prefix is
   --  replaced by the running program name.

   Log_File_Directory        : constant String := "./";

   Log_Split_Mode            : constant String := "NONE";
   Log_Filename_Prefix       : constant String := "@";

   Error_Log_Split_Mode      : constant String := "NONE";
   Error_Log_Filename_Prefix : constant String := "@_error";

   --  All times are in seconds

   One_Hour     : constant := 3_600.0;
   One_Minute   : constant :=    60.0;

   Eight_Hours  : constant :=  8.0 * One_Hour;
   Three_Hours  : constant :=  3.0 * One_Hour;
   Five_Minutes : constant :=  5.0 * One_Minute;
   Ten_Minutes  : constant := 10.0 * One_Minute;

   Session_Cleanup_Interval        : constant Duration := Five_Minutes;
   Session_Lifetime                : constant Duration := Ten_Minutes;

   Cleaner_Wait_For_Client_Timeout : constant Duration := 80.0;
   Cleaner_Client_Header_Timeout   : constant Duration := 20.0;
   Cleaner_Client_Data_Timeout     : constant Duration := Eight_Hours;
   Cleaner_Server_Response_Timeout : constant Duration := Eight_Hours;

   Force_Wait_For_Client_Timeout   : constant Duration := 2.0;
   Force_Client_Header_Timeout     : constant Duration := 3.0;
   Force_Client_Data_Timeout       : constant Duration := Three_Hours;
   Force_Server_Response_Timeout   : constant Duration := Three_Hours;

   Send_Timeout    : constant Duration := 40.0;
   Receive_Timeout : constant Duration := 30.0;

   Status_Page     : constant String := "aws_status.thtml";
   Up_Image        : constant String := "aws_up.png";
   Down_Image      : constant String := "aws_down.png";
   Logo_Image      : constant String := "aws_logo.png";

   Security                  : constant Boolean := False;
   Certificate               : constant String  := "cert.pem";
   Session                   : constant Boolean := False;
   Case_Sensitive_Parameters : constant Boolean := True;
   Check_URL_Validity        : constant Boolean := True;
   Line_Stack_Size           : constant         := 16#150_000#;

end AWS.Default;
