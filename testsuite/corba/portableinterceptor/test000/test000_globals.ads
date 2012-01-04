------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      T E S T 0 0 0 _ G L O B A L S                       --
--                                                                          --
--                                 S p e c                                  --
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

with Test000_Idl.ClientInterceptor;
with Test000_Idl.ServerInterceptor;
with Test000_Idl.TestInterface;

package Test000_Globals is

   Client_A : Test000_Idl.ClientInterceptor.Local_Ref;
   Client_B : Test000_Idl.ClientInterceptor.Local_Ref;
   Client_C : Test000_Idl.ClientInterceptor.Local_Ref;
   Server_A : Test000_Idl.ServerInterceptor.Local_Ref;
   Server_B : Test000_Idl.ServerInterceptor.Local_Ref;
   Server_C : Test000_Idl.ServerInterceptor.Local_Ref;

   Object_1 : Test000_Idl.TestInterface.Ref;

   procedure Enable_Client_Interceptors;

   procedure Disable_Client_Interceptors;

   procedure Enable_Server_Interceptors;

   procedure Disable_Server_Interceptors;

   type Log_Source is (Client, Object, Server);

   type Log_Record (Source : Log_Source := Object) is record
      Name : Character;

      case Source is
         when Object =>
            null;

         when Client =>
            Client_Point :
              Test000_Idl.ClientInterceptor.Client_Interception_Point;

         when Server =>
            Server_Point :
              Test000_Idl.ServerInterceptor.Server_Interception_Point;
      end case;
   end record;

   type Log_Array is array (Positive range <>) of Log_Record;

   procedure Log_Point
     (Name : String);

   procedure Log_Point
     (Name  : String;
      Point : Test000_Idl.ClientInterceptor.Client_Interception_Point);

   procedure Log_Point
     (Name  : String;
      Point : Test000_Idl.ServerInterceptor.Server_Interception_Point);

   procedure Clear_Log;

   function Get_Log return Log_Array;

   procedure Output (Log : Log_Array);

end Test000_Globals;
