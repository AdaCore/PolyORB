------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      T E S T 0 0 0 _ G L O B A L S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
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
