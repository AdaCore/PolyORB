------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
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
