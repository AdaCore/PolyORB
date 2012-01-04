------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      T E S T 0 0 1 _ G L O B A L S                       --
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

with CORBA;
with IOP.Codec;
with PortableInterceptor;

with Test001_Interface;

package Test001_Globals is

   Pass_Not_Implemented : Boolean := True;
   --  Iff set to True then all non implemented tests successfully passed.

   type Interception_Point is
     (Send_Request,
      Send_Poll,
      Receive_Reply,
      Receive_Exception,
      Receive_Other,
      Receive_Request_Service_Contexts,
      Receive_Request,
      Send_Reply,
      Send_Exception,
      Send_Other);

   subtype Client_Interception_Point is Interception_Point
      range Send_Request .. Receive_Other;

   subtype Server_Interception_Point is Interception_Point
      range Receive_Request_Service_Contexts .. Send_Other;

   Test_Object   : Test001_Interface.Ref;
   Test_ObjectId : PortableInterceptor.ObjectId;

   Test_Forward_Object : Test001_Interface.Ref;

   Test_Codec : IOP.Codec.Local_Ref;

   Test_Request_Context : IOP.ServiceContext;
   Test_Reply_Context   : IOP.ServiceContext;

   Test_Client_Request_Id : CORBA.Unsigned_Long;
   Test_Server_Request_Id : CORBA.Unsigned_Long;

   --  Test behavior switches

   Raise_Test_Exception : Boolean := False;
   Forward_Location     : Boolean := False;
   Enable_Test_Point    : array (Interception_Point) of Boolean
     := (others => False);

   function Image (Value : Interception_Point) return String;

   procedure Output
     (Point     : Interception_Point;
      Operation : String;
      Status    : Boolean;
      Comment   : String := "");

end Test001_Globals;
