------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             T Y P E D T E S T _ I N T E R F A C E . I M P L              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with TypedTest_Interface;

with TypedTest_Interface.Skel;
pragma Warnings (Off, TypedTest_Interface.Skel);

with TypedTest_Interface.Helper;
pragma Warnings (Off, TypedTest_Interface.Helper);

with PolyORB.CORBA_P.Server_Tools;

package body TypedTest_Interface.Impl is

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;

   function EchoString
     (Self : access Object; Mesg : CORBA.String)
     return CORBA.String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      Ada.Text_IO.Put_Line
        ("Echoing string: « " & CORBA.To_Standard_String (Mesg)
         & " »");
      return Mesg;
   end EchoString;

   function Create
      return CORBA.Impl.Object_Ptr
   is
      TypedTest_Int : Object_Ptr;
      My_Ref        : TypedTest_Interface.Ref;
   begin
      TypedTest_Int := new Object;
      Initiate_Servant (Servant (TypedTest_Int), My_Ref);
      return CORBA.Impl.Object_Ptr (TypedTest_Int);
   end Create;

end TypedTest_Interface.Impl;
