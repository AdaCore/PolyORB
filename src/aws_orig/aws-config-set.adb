------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A W S . C O N F I G . S E T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2006, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

package body AWS.Config.Set is

   procedure Parameter
     (Param_Set     : in out Parameter_Set;
      Name, Value   : String;
      Error_Context : String);
   --  Set parameter Name/Value in Param_Set. Raises Constraint_Error with
   --  Error_Context added to error message if Name / Value is wrong.

   -----------------------
   -- Accept_Queue_Size --
   -----------------------

   procedure Accept_Queue_Size (O : in out Object; Value : Positive) is
   begin
      O.P (Accept_Queue_Size).Pos_Value := Value;
   end Accept_Queue_Size;

   ---------------
   -- Admin_URI --
   ---------------

   procedure Admin_URI (O : in out Object; Value : String) is
   begin
      O.P (Admin_URI).Str_Value := To_Unbounded_String (Value);
   end Admin_URI;

   -------------------------------
   -- Case_Sensitive_Parameters --
   -------------------------------

   procedure Case_Sensitive_Parameters
     (O     : in out Object;
      Value : Boolean) is
   begin
      O.P (Case_Sensitive_Parameters).Bool_Value := Value;
   end Case_Sensitive_Parameters;

   -----------------
   -- Certificate --
   -----------------

   procedure Certificate (Filename : String) is
   begin
      Process_Options (Certificate).Str_Value
        := To_Unbounded_String (Filename);
   end Certificate;

   ---------------------------------
   -- Cleaner_Client_Data_Timeout --
   ---------------------------------

   procedure Cleaner_Client_Data_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Cleaner_Client_Data_Timeout).Dur_Value := Value;
   end Cleaner_Client_Data_Timeout;

   -----------------------------------
   -- Cleaner_Client_Header_Timeout --
   -----------------------------------

   procedure Cleaner_Client_Header_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Cleaner_Client_Header_Timeout).Dur_Value := Value;
   end Cleaner_Client_Header_Timeout;

   -------------------------------------
   -- Cleaner_Server_Response_Timeout --
   -------------------------------------

   procedure Cleaner_Server_Response_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Cleaner_Server_Response_Timeout).Dur_Value := Value;
   end Cleaner_Server_Response_Timeout;

   -------------------------------------
   -- Cleaner_Wait_For_Client_Timeout --
   -------------------------------------

   procedure Cleaner_Wait_For_Client_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Cleaner_Wait_For_Client_Timeout).Dur_Value := Value;
   end Cleaner_Wait_For_Client_Timeout;

   ----------------
   -- Down_Image --
   ----------------

   procedure Down_Image (O : in out Object; Value : String) is
   begin
      O.P (Down_Image).Str_Value := To_Unbounded_String (Value);
   end Down_Image;

   -------------------------------
   -- Error_Log_Filename_Prefix --
   -------------------------------

   procedure Error_Log_Filename_Prefix
     (O : in out Object; Value : String) is
   begin
      O.P (Error_Log_Filename_Prefix).Str_Value := To_Unbounded_String (Value);
   end Error_Log_Filename_Prefix;

   --------------------------
   -- Error_Log_Split_Mode --
   --------------------------

   procedure Error_Log_Split_Mode (O : in out Object; Value : String) is
   begin
      O.P (Error_Log_Split_Mode).Str_Value := To_Unbounded_String (Value);
   end Error_Log_Split_Mode;

   -------------------------------
   -- Force_Client_Data_Timeout --
   -------------------------------

   procedure Force_Client_Data_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Force_Client_Data_Timeout).Dur_Value := Value;
   end Force_Client_Data_Timeout;

   ---------------------------------
   -- Force_Client_Header_Timeout --
   ---------------------------------

   procedure Force_Client_Header_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Force_Client_Header_Timeout).Dur_Value := Value;
   end Force_Client_Header_Timeout;

   -----------------------------------
   -- Force_Server_Response_Timeout --
   -----------------------------------

   procedure Force_Server_Response_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Force_Server_Response_Timeout).Dur_Value := Value;
   end Force_Server_Response_Timeout;

   -----------------------------------
   -- Force_Wait_For_Client_Timeout --
   -----------------------------------

   procedure Force_Wait_For_Client_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Force_Wait_For_Client_Timeout).Dur_Value := Value;
   end Force_Wait_For_Client_Timeout;

   ------------------
   -- Hotplug_Port --
   ------------------

   procedure Hotplug_Port (O : in out Object; Value : Positive) is
   begin
      O.P (Hotplug_Port).Pos_Value := Value;
   end Hotplug_Port;

   ---------------------
   -- Line_Stack_Size --
   ---------------------

   procedure Line_Stack_Size (O : in out Object; Value : Positive) is
   begin
      O.P (Line_Stack_Size).Pos_Value := Value;
   end Line_Stack_Size;

   ------------------------
   -- Log_File_Directory --
   ------------------------

   procedure Log_File_Directory (O : in out Object; Value : String) is
   begin
      O.P (Log_File_Directory).Dir_Value := To_Unbounded_String (Value);
   end Log_File_Directory;

   -------------------------
   -- Log_Filename_Prefix --
   -------------------------

   procedure Log_Filename_Prefix (O : in out Object; Value : String) is
   begin
      O.P (Log_Filename_Prefix).Str_Value := To_Unbounded_String (Value);
   end Log_Filename_Prefix;

   --------------------
   -- Log_Split_Mode --
   --------------------

   procedure Log_Split_Mode (O : in out Object; Value : String) is
   begin
      O.P (Log_Split_Mode).Str_Value := To_Unbounded_String (Value);
   end Log_Split_Mode;

   ----------------
   -- Logo_Image --
   ----------------

   procedure Logo_Image (O : in out Object; Value : String) is
   begin
      O.P (Logo_Image).Str_Value := To_Unbounded_String (Value);
   end Logo_Image;

   --------------------
   -- Max_Connection --
   --------------------

   procedure Max_Connection (O : in out Object; Value : Positive) is
   begin
      O.P (Max_Connection).Pos_Value := Value;
   end Max_Connection;

   ---------------
   -- Parameter --
   ---------------

   procedure Parameter
     (Config        : in out Object;
      Name          : String;
      Value         : String;
      Error_Context : String := "") is
   begin
      Parameter (Config.P, Name, Value, Error_Context);
   end Parameter;

   procedure Parameter
     (Name          : String;
      Value         : String;
      Error_Context : String := "") is
   begin
      Parameter (Process_Options, Name, Value, Error_Context);
   end Parameter;

   procedure Parameter
     (Param_Set     : in out Parameter_Set;
      Name, Value   : String;
      Error_Context : String)
   is
      P : Parameter_Name;

      procedure Set_Parameter (Param : in out Values);
      --  Set parameter depending on the type (Param.Kind).

      procedure Error (Message : String);
      --  Raises Constraint_Error with associated message and Error_Context
      --  string.

      function "+" (S : String)
        return Unbounded_String
        renames To_Unbounded_String;

      -----------
      -- Error --
      -----------

      procedure Error (Message : String) is
      begin
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            Error_Context & ASCII.LF & Message & '.');
      end Error;

      Expected_Type : Unbounded_String;

      -------------------
      -- Set_Parameter --
      -------------------

      procedure Set_Parameter (Param : in out Values) is
      begin
         case Param.Kind is
            when Str =>
               Expected_Type := +"string";
               Param.Str_Value := +Value;

            when Dir =>
               Expected_Type := +"string";

               if Value (Value'Last) = '/'
                 or else Value (Value'Last) = '\'
               then
                  Param.Dir_Value := +Value;
               else
                  Param.Dir_Value := +(Value & '/');
               end if;

            when Pos =>
               Expected_Type := +"positive";
               Param.Pos_Value := Positive'Value (Value);

            when Dur =>
               Expected_Type := +"duration";
               Param.Dur_Value := Duration'Value (Value);

            when Bool =>
               Expected_Type := +"boolean";
               Param.Bool_Value := Boolean'Value (Value);
         end case;
      end Set_Parameter;

   begin
      begin
         P := Parameter_Name'Value (Name);
      exception
         when others =>
            Error ("unrecognized option " & Name);
            return;
      end;

      if P not in Param_Set'Range then
         declare
            Not_Supported_Msg : constant String
              := " option '" & Name
              & "' not supported for this configuration context.";
         begin
            if P in Process_Parameter_Name'Range then
               Error ("Per process" & Not_Supported_Msg);
            else
               Error ("Per server" & Not_Supported_Msg);
            end if;
         end;
         return;
      else
         Set_Parameter (Param_Set (P));
      end if;

   exception
      when others =>
         Error
           ("wrong value for " & Name
            & " " & To_String (Expected_Type) & " expected");
   end Parameter;

   ---------------------
   -- Receive_Timeout --
   ---------------------

   procedure Receive_Timeout (O : in out Object; Value : Duration) is
   begin
      O.P (Receive_Timeout).Dur_Value := Value;
   end Receive_Timeout;

   --------------
   -- Security --
   --------------

   procedure Security (O : in out Object; Value : Boolean) is
   begin
      O.P (Security).Bool_Value := Value;
   end Security;

   ------------------
   -- Send_Timeout --
   ------------------

   procedure Send_Timeout (O : in out Object; Value : Duration) is
   begin
      O.P (Send_Timeout).Dur_Value := Value;
   end Send_Timeout;

   -----------------
   -- Server_Host --
   -----------------

   procedure Server_Host (O : in out Object; Value : String) is
   begin
      O.P (Server_Host).Str_Value := To_Unbounded_String (Value);
   end Server_Host;

   -----------------
   -- Server_Name --
   -----------------

   procedure Server_Name (O : in out Object; Value : String) is
   begin
      O.P (Server_Name).Str_Value := To_Unbounded_String (Value);
   end Server_Name;

   -----------------
   -- Server_Port --
   -----------------

   procedure Server_Port (O : in out Object; Value : Positive) is
   begin
      O.P (Server_Port).Pos_Value := Value;
   end Server_Port;

   -------------
   -- Session --
   -------------

   procedure Session (O : in out Object; Value : Boolean) is
   begin
      O.P (Session).Bool_Value := Value;
   end Session;

   ------------------------------
   -- Session_Cleanup_Interval --
   ------------------------------

   procedure Session_Cleanup_Interval
     (Value : Duration) is
   begin
      Process_Options (Session_Cleanup_Interval).Dur_Value := Value;
   end Session_Cleanup_Interval;

   ----------------------
   -- Session_Lifetime --
   ----------------------

   procedure Session_Lifetime (Value : Duration) is
   begin
      Process_Options (Session_Lifetime).Dur_Value := Value;
   end Session_Lifetime;

   -----------------
   -- Status_Page --
   -----------------

   procedure Status_Page (O : in out Object; Value : String) is
   begin
      O.P (Status_Page).Str_Value := To_Unbounded_String (Value);
   end Status_Page;

   --------------
   -- Up_Image --
   --------------

   procedure Up_Image (O : in out Object; Value : String) is
   begin
      O.P (Up_Image).Str_Value := To_Unbounded_String (Value);
   end Up_Image;

   ----------------------
   -- Upload_Directory --
   ----------------------

   procedure Upload_Directory (O : in out Object; Value : String) is
      Last : constant Character := Value (Value'Last);
   begin
      if Last = '/' or else Last = '\' then
         O.P (Upload_Directory).Dir_Value := To_Unbounded_String (Value);
      else
         O.P (Upload_Directory).Dir_Value
           := To_Unbounded_String (Value & '/');
      end if;
   end Upload_Directory;

   --------------
   -- WWW_Root --
   --------------

   procedure WWW_Root (O : in out Object; Value : String) is
   begin
      O.P (WWW_Root).Dir_Value := To_Unbounded_String (Value);
   end WWW_Root;

end AWS.Config.Set;
