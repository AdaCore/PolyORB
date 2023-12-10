------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            R U N _ T E S T S                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2023, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with CORBA; use CORBA;
with all_functions; use all_functions;
with PolyORB.Utils.Report;

--  Remove no previous spec warning
procedure Run_Tests (MyObj : all_functions.Ref) is
   use PolyORB.Utils.Report;
   I, J, K, L, M : CORBA.Short;
   Ok : Boolean;
begin
   New_Test ("Different invocation modes");

   Output ("test not nil reference", not Is_Nil (MyObj));

--    Set_The_Attribute (MyObj, 24);
--    Output ("test attribute", Get_The_Attribute (MyObj) = 24);

--    Output ("test readonly attribute",
--      Get_The_Readonly_Attribute (MyObj) = 18);

   begin
      Ok := True;
      void_proc (MyObj);
   exception when others =>
      Ok := False;
   end;
   Output ("test void procedure", Ok);

   begin
      in_proc (MyObj, 1, 2, 3);
      Ok := True;
   exception when others =>
      Ok := False;
   end;
   Output ("test in param procedure", Ok);

   begin
      Ok := False;
      out_proc (MyObj, I, J, K);
      Ok := (I = 10) and then (J = 11) and then (K = 12);
   exception when others =>
      null;
   end;
   Output ("test out param procedure", Ok);

   begin
      Ok := False;
      out_in_proc (MyObj, I, 41);
      Ok := I = 42;
   exception when others =>
      null;
   end;
   Output ("test out in param procedure", Ok);

   begin
      Ok := False;
      I  := 2;
      J  := 3;
      inout_proc (MyObj, I, J);
      Ok := (I = 3 and then J = 4);
   exception when others =>
      null;
   end;
   Output ("test in out param procedure", Ok);

   begin
      Ok := False;
      I := 1;
      J := 2;
      in_out_proc (MyObj, 1, 2, I, J);
      Ok := (I = 3 and then J = 4);
   exception when others =>
      null;
   end;
   Output ("test in and out param procedure", Ok);

   begin
      Ok := False;
      I  := -4;
      J  := -5;
      in_inout_proc (MyObj, 1, I, 3, J);
      Ok := (I = 36) and then (J = 40);
   exception when others =>
      null;
   end;
   Output ("test in and inout param procedure", Ok);

   begin
      I := -11;
      J := 123;
      K := 456;
      L := -41;
      out_inout_proc (MyObj, I, J, K, L);
      Ok := (I = 111) and then (J = 457) and then (K = 124) and then (L = 999);
   exception when others =>
      null;
   end;
   Output ("test inout and out param procedure", Ok);

   begin
      Ok := False;
      I := 78;
      J := 79;
      in_out_inout_proc (MyObj, 1, I, J);
      Ok := (I = -54) and then (J = 80);
   exception when others =>
      null;
   end;
   Output ("test in and out and inout param procedure", Ok);

   Output ("test void function", void_fun (MyObj) = 3);
   Output ("test in param function", in_fun (MyObj, 1, 2, 3) = 7);

   begin
      Ok := False;
      I := 1;
      J := 2;
      K := 3;
      L := 4;
      out_fun (MyObj, I, J, K, L);
      Ok := (I = 5) and then (J = 6) and then (K = 7) and then (L = 10);
   exception when others =>
      null;
   end;
   Output ("test out param function", Ok);

   begin
      Ok := False;
      I := 1;
      J := 2;
      K := 3;
      inout_fun (MyObj, I, J, L);
      Ok := (I = 2) and then (J = 3) and then (L = 5);
   exception when others =>
      null;
   end;
   Output ("test inout param function", Ok);

   begin
      Ok := False;
      I := 10;
      J := 11;
      in_out_fun (MyObj, 1, 2, I, J, K);
      Ok := (I = 2) and then (J = 1) and then (K = 3);
   exception when others =>
      null;
   end;
   Output ("test in and out param function", Ok);

   begin
      Ok := False;
      I := -1;
      J := -2;
      K := -3;
      in_inout_fun (MyObj, -1, I, -2, J, K);
      Ok := (I = -2) and then (J = -4) and then (K = -6);
   exception when others =>
      null;
   end;
   Output ("test in and inout param function", Ok);

   begin
      Ok := False;
      I := -1;
      J := -2;
      K := -3;
      L := -4;
      M := -5;
      out_inout_fun (MyObj, I, J, K, L, M);
      Ok := (I = -2) and then (J = -1) and then (K = -2)
        and then (L = -3) and then (M = -7);
   exception when others =>
      null;
   end;
   Output ("test out and inout param function", Ok);

   begin
      Ok := False;
      I := -1;
      J := -2;
      K := -3;
      in_out_inout_fun (MyObj, 85, I, J, K);
      Ok := (I = 86) and then (J = 83) and then (K = -1);
   exception when others =>
      null;
   end;
   Output ("test in and out and inout param function", Ok);

   begin
      oneway_void_proc (MyObj);
      delay 0.5;
      Ok := oneway_checker (MyObj) = 1;
      if Ok then
         delay 1.0;
         Ok := oneway_checker (MyObj) = 2;
      end if;
   exception when others =>
      Ok := False;
   end;
   Output ("test void one way procedure", Ok);

   declare
      S : Short;
   begin
      oneway_in_proc (MyObj, 10, 20);
      delay 0.5;
      S := oneway_checker (MyObj);
      Ok := S = 10;
      if Ok then
         delay 1.0;
         S := oneway_checker (MyObj);
         Ok := S = 20;
      end if;
   exception when others =>
      Ok := False;
   end;
   Output ("test in param one way procedure", Ok);

   begin
      StopServer (MyObj);
      Ok := True;
   exception when others =>
      Ok := False;
      raise;
   end;

   Output ("shut down server", Ok);
   End_Report;
end Run_Tests;
