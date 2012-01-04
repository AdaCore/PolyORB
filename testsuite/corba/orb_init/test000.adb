------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
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

with CORBA.ORB;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with PolyORB.Utils.Report; use PolyORB.Utils.Report;

procedure Test000 is
   use CORBA.ORB;
   use CORBA;

begin
   New_Test ("ORB_Init");

   --  Test 1

   declare
      Argv : Arg_List := Command_Line_Arguments;

   begin
      CORBA.ORB.Init (To_CORBA_String ("ORB"), Argv);
      Output ("Default initialisation", True);
   end;

   --  Test 2: Initialisation with bad suffix

   declare
      Argv : Arg_List;

   begin
      Append (Argv, To_CORBA_String ("-ORBtoto"));
      Append (Argv, To_CORBA_String ("truc"));

      CORBA.ORB.Init (To_CORBA_String ("ORB"), Argv);
      Output ("Initialisation with bad suffix", False);
   exception
      when CORBA.Bad_Param =>
         Output ("Initialisation with bad suffix", True);
   end;

   --  Test 3: Initialisation with bad reference

   declare
      Argv : Arg_List;

   begin
      Append (Argv, To_CORBA_String ("-ORBInitRef"));
      Append (Argv, To_CORBA_String ("truc=bidule"));

      CORBA.ORB.Init (To_CORBA_String ("ORB"), Argv);
      Output ("Initialisation with bad reference", False);

   exception
      when CORBA.Bad_Param =>
         Output ("Initialisation with bad reference", True);
   end;

   --  Test 4: Initial reference set up

   declare
      Argv : Arg_List;

   begin
      Append (Argv, To_CORBA_String ("-ORBInitRef"));
      Append
        (Argv,
         To_CORBA_String
         ("truc=corbaloc:miop:1.0@1.0-TestDomain-5506/239.239.239.18:5678"));

      Append (Argv, To_CORBA_String ("foo"));

      CORBA.ORB.Init (To_CORBA_String ("ORB"), Argv);

      Output ("Initial reference set up", True);
      Output ("Length is correct", Length (Argv) = 1);
   end;

   --  Test 5: Initialisation with no reference

   declare
      Argv : Arg_List;

   begin
      Append (Argv, To_CORBA_String ("-ORBInitRef"));
      Append (Argv, To_CORBA_String ("truc"));

      CORBA.ORB.Init (To_CORBA_String ("ORB"), Argv);
      Output ("Initialisation with no reference", False);

   exception
      when CORBA.Bad_Param =>
         Output ("Initialisation with no reference", True);
   end;

   --  Test 6: Initialisation with no parameter

   declare
      Argv : Arg_List;

   begin
      Append (Argv, To_CORBA_String ("-ORBInitRef"));

      CORBA.ORB.Init (To_CORBA_String ("ORB"), Argv);
      Output ("Initialisation with no parameter", False);

   exception
      when CORBA.Bad_Param =>
         Output ("Initialisation with no parameter", True);
   end;

   --  Test 7: Initial reference set up

   declare
      Argv : Arg_List;

   begin
      Append (Argv,
              To_CORBA_String
              ("-ORBInitReftr=corbaloc:miop:1.0@1.0-T-5/239.239.239.18:58"));

      CORBA.ORB.Init (To_CORBA_String ("ORB"), Argv);

      Output ("Initial reference set up", True);
      Output ("Length is correct", Length (Argv) = 0);
   end;

   End_Report;
end Test000;
