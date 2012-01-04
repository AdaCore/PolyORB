------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      T E S T 0 0 0 _ G L O B A L S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;
with PolyORB.Utils.Chained_Lists;

package body Test000_Globals is

   package Log_Lists is new PolyORB.Utils.Chained_Lists (Log_Record);

   Log : Log_Lists.List;

   ---------------
   -- Clear_Log --
   ---------------

   procedure Clear_Log is
   begin
      Log := Log_Lists.Empty;
   end Clear_Log;

   ---------------------------------
   -- Disable_Client_Interceptors --
   ---------------------------------

   procedure Disable_Client_Interceptors is
   begin
      Test000_Idl.ClientInterceptor.Disable (Client_A);
      Test000_Idl.ClientInterceptor.Disable (Client_B);
      Test000_Idl.ClientInterceptor.Disable (Client_C);
   end Disable_Client_Interceptors;

   ---------------------------------
   -- Disable_Server_Interceptors --
   ---------------------------------

   procedure Disable_Server_Interceptors is
   begin
      Test000_Idl.ServerInterceptor.Disable (Server_A);
      Test000_Idl.ServerInterceptor.Disable (Server_B);
      Test000_Idl.ServerInterceptor.Disable (Server_C);
   end Disable_Server_Interceptors;

   --------------------------------
   -- Enable_Client_Interceptors --
   --------------------------------

   procedure Enable_Client_Interceptors is
   begin
      Test000_Idl.ClientInterceptor.Enable (Client_A);
      Test000_Idl.ClientInterceptor.Enable (Client_B);
      Test000_Idl.ClientInterceptor.Enable (Client_C);
   end Enable_Client_Interceptors;

   --------------------------------
   -- Enable_Server_Interceptors --
   --------------------------------

   procedure Enable_Server_Interceptors is
   begin
      Test000_Idl.ServerInterceptor.Enable (Server_A);
      Test000_Idl.ServerInterceptor.Enable (Server_B);
      Test000_Idl.ServerInterceptor.Enable (Server_C);
   end Enable_Server_Interceptors;

   -------------
   -- Get_Log --
   -------------

   function Get_Log return Log_Array is
      Result : Log_Array (1 .. Log_Lists.Length (Log));
   begin
      for J in Result'Range loop
         Result (J) := Log_Lists.Element (Log, J - 1).all;
      end loop;

      return Result;
   end Get_Log;

   ---------------
   -- Log_Point --
   ---------------

   procedure Log_Point
     (Name : String)
   is
   begin
      Log_Lists.Append (Log, (Object, Name (Name'First)));
   end Log_Point;

   ---------------
   -- Log_Point --
   ---------------

   procedure Log_Point
     (Name  : String;
      Point : Test000_Idl.ClientInterceptor.Client_Interception_Point)
   is
   begin
      Log_Lists.Append (Log, (Client, Name (Name'First), Point));
   end Log_Point;

   ---------------
   -- Log_Point --
   ---------------

   procedure Log_Point
     (Name  : String;
      Point : Test000_Idl.ServerInterceptor.Server_Interception_Point)
   is
   begin
      Log_Lists.Append (Log, (Server, Name (Name'First), Point));
   end Log_Point;

   ------------
   -- Output --
   ------------

   procedure Output (Log : Log_Array) is
      use Ada.Text_IO;
   begin
      for J in Log'Range loop
         Put (Log_Source'Image (Log (J).Source));
         Put (' ');
         Put (Log (J).Name);
         Put (' ');

         case Log (J).Source is
            when Client =>
               Put
                (Test000_Idl.ClientInterceptor.Client_Interception_Point'Image
                  (Log (J).Client_Point));

            when Server =>
               Put
                (Test000_Idl.ServerInterceptor.Server_Interception_Point'Image
                  (Log (J).Server_Point));

            when Object =>
               null;
         end case;

         New_Line;
      end loop;
   end Output;

end Test000_Globals;
