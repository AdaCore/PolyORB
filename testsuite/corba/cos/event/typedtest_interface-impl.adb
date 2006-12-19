------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               T Y P E D T E S T_I N T E R F A C E. I M P L               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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
     (Self : access Object; Mesg : in CORBA.String)
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
