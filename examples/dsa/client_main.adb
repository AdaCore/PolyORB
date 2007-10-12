------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C L I E N T _ M A I N                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with RCI;
with RT;
with SP;
with Matrices;

with System.RPC;

procedure Client_Main is
   S : constant String := "Hello DSA world!";
   RAS : RCI.echo_RAS;

   procedure Try_RACW (Name : String);
   procedure Try_RACW (Name : String) is
      use type RT.RACW;
      Obj : RT.RACW;
   begin
      Put_Line ("Trying RACW with Name = """ & Name & """");
      Obj := RCI.Get_Obj (Name);
      if Obj = null then
         Put_Line ("Got null!");
      else
         Put_Line ("Got not null: " & RT.Tekitoa (Obj.all) & " is alive!");
      end if;
   end Try_RACW;

   Z : constant RCI.Complex := (Re => 2.0, Im => 3.0);

begin
   SP.Shared_Integer := 42;
   Put_Line ("I said: " & S);
   Put_Line ("The server (on partition"
     & System.RPC.Partition_ID'Image (RCI'Partition_Id)
     & ") replied: "
     & RCI.echoString (S));
   RAS := RCI.echoString'Access;
   Put_Line ("Obtained RAS on client");
   Put_Line ("through RAS: " & RAS (S & " (RASI)"));
   Put_Line ("through RAS: " & RAS.all (S & " (RASE)"));
   RCI.Check_Back_RAS (RAS, "RAS taken on client");

   RAS := RCI.getRAS;
   Put_Line ("Obtained RAS-from-server");
   Put_Line ("through RAS-from-server: " & RAS (S & " (RASS)"));
   RCI.Check_Back_RAS (RAS, "RAS taken on server");

   Try_RACW ("");
   Try_RACW ("Elvis");

   declare
      use type RCI.Vector;

      V : constant RCI.Vector := (3 => 111, 4 => 222, 5 => 333);
   begin
      Put_Line ("V passed? " & Boolean'Image (V = RCI.echoVector (V)));
   end;

   Put_Line ("|2 + 3i|^2 = " & Float'Image (RCI.Modulus2 (Z)));

   declare
      use type RCI.C_4_5;

      Matrix : RCI.C_4_5;
   begin
      for J in Matrix'Range (1) loop
         for K in Matrix'Range (2) loop
            Matrix (J, K) := (Re => 1.0 / Float (J), Im => 1.0 / Float (K));
         end loop;
      end loop;
      Put_Line ("Constrained matrix passed? "
        & Boolean'Image (Matrix = RCI.echoC_4_5 (Matrix)));
   end;

   declare
      use Matrices;

      M : Matrix (8 .. 10, 3 .. 11);
   begin
      for J in M'Range (1) loop
         for K in M'Range (2) loop
            M (J, K) := Float (J) + 0.01 * Float (K);
         end loop;
      end loop;

      Put_Line ("Sending matrix:");
      for J in M'Range (1) loop
         for K in M'Range (2) loop
            Put (" " & Float'Image (M (J, K)));
         end loop;
         New_Line;
      end loop;

      declare
         M_Prime : constant Matrix := RCI.echoTranspose (M);
      begin
         Put_Line ("Ranges of M : (" & Integer'Image (M'First (1))
                             & ".." & Integer'Image (M'Last (1))
                             & ", " & Integer'Image (M'First (2))
                             & ".." & Integer'Image (M'Last (2)) & ")");

         Put_Line ("Ranges of M': (" & Integer'Image (M_Prime'First (1))
                              & ".." & Integer'Image (M_Prime'Last (1))
                              & ", " & Integer'Image (M_Prime'First (2))
                              & ".." & Integer'Image (M_Prime'Last (2)) & ")");

         Put_Line ("Unconstrained matrix passed? "
                   & Boolean'Image (M = Transpose (M_Prime)));
      end;
   end;

   declare
      C : constant Integer := RCI.Get_Cookie;
   begin
      Put_Line ("Cookie value:" & Integer'Image (C));
      RCI.Delayed_Set_Cookie (C + 1);
   end;
   delay until Clock + Milliseconds (500);
   Put_Line ("Cookie value after 0.5 s:" & Integer'Image (RCI.Get_Cookie));
   delay until Clock + Milliseconds (2_500);
   Put_Line ("Cookie value after 3 s:" & Integer'Image (RCI.Get_Cookie));

   begin
      Put ("Raise_Program_Error: ");
      RCI.Raise_Program_Error;
      Put_Line ("no exception.");
   exception
      when E : others =>
         Put_Line ("raised " & Ada.Exceptions.Exception_Name (E));
   end;

   begin
      Put ("Raise_Visible: ");
      RCI.Raise_Visible;
      Put_Line ("no exception.");
   exception
      when E : others =>
         Put_Line ("raised " & Ada.Exceptions.Exception_Name (E));
   end;

   begin
      Put ("Raise_Invisible: ");
      RCI.Raise_Invisible;
      Put_Line ("no exception.");
   exception
      when E : others =>
         Put_Line ("raised " & Ada.Exceptions.Exception_Name (E));
   end;
end Client_Main;
