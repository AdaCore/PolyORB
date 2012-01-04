------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
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

with Ada.Command_Line;
with Ada.Exceptions;

with CORBA.ORB;
with PolyORB.Utils.Report;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with Test_Interface;

procedure Client is
   use PolyORB.Utils.Report;

   Argv : CORBA.ORB.Arg_List := CORBA.ORB.Command_Line_Arguments;
   Ref  : Test_Interface.Ref;

begin
   CORBA.ORB.Init (CORBA.ORB.To_CORBA_String ("ORB"), Argv);

   CORBA.ORB.String_To_Object
     (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)), Ref);

   New_Test ("Raising CODESET_INCOMPATIBLE exception");

   begin
      Test_Interface.Put (Ref, CORBA.To_CORBA_Wide_String ("Test"));
      Output ("Raising CODESET_INCOMPATIBLE", False);
   exception
      when CORBA.Codeset_Incompatible =>
         Output ("Raising CODESET_INCOMPATIBLE", True);
      when E : others =>
         Output ("Raising " & Ada.Exceptions.Exception_Information (E), False);
   end;

   End_Report;
end Client;
