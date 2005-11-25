------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      T E S T 0 0 0 _ G L O B A L S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
     (Name : in String);

   procedure Log_Point
     (Name  : in String;
      Point : in Test000_Idl.ClientInterceptor.Client_Interception_Point);

   procedure Log_Point
     (Name  : in String;
      Point : in Test000_Idl.ServerInterceptor.Server_Interception_Point);

   procedure Clear_Log;

   function Get_Log return Log_Array;

   procedure Output (Log : in Log_Array);

end Test000_Globals;
