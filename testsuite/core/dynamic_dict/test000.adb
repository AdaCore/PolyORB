------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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

--  $Id$

with PolyORB.Dynamic_Dict;

with PolyORB.Utils.Report;

procedure Test000 is

   use PolyORB.Utils.Report;

   type String_Access is access all String;

   package My_Dict is new PolyORB.Dynamic_Dict (Value => String_Access);

   ---------------------
   -- Test_Regression --
   ---------------------

   procedure Test_Regression;
   --  This particular configuration raised an exception.

   procedure Test_Regression
   is

      Values : constant array (Positive range <>) of String_Access :=
        (new String'("tasking.profiles.full_tasking.threads"),
         new String'("tasking.threads"),
         new String'("tasking.profiles.full_tasking.mutexes"),
         new String'("tasking.mutexes"),
         new String'("tasking.profiles.full_tasking.condition_variables"),
         new String'("tasking.condition_variables"),
         new String'("exceptions.stack"),
         new String'("exceptions"),
         new String'("smart_pointers"),
         new String'("binding_data.iiop"),
         new String'("protocols.giop"),
         new String'("orb.thread_pool"),
         new String'("orb.tasking_policy"),
         new String'("orb.threads_init"),
         new String'("orb.tasking_policy_init"),
         new String'("orb"),
         new String'("corba.orb"),
         new String'("corba.initial_references"),
         new String'("tcp_access_points.corba"),
         new String'("tcp_access_points.srp"),
         new String'("tasking.soft_links"),
         new String'("soft_links"));

      Result : String_Access;
   begin
      for J in Values'Range loop
         My_Dict.Register (Values (J).all, new String'("foo"));

         for K in Values'First .. J loop
            Result := My_Dict.Lookup (Values (K).all, Default => null);

            if Result = null
              or else Result.all /= "foo" then

               Output ("Regression occured for key "
                       & Values (K).all
                       & " at stage #"
                       & Integer'Image (J),
                       False);
               raise Program_Error;
            end if;
         end loop;
      end loop;

      for J in Values'Range loop
         My_Dict.Unregister (Values (J).all);
      end loop;

      Output ("Regression did not occured", True);
   exception
      when others =>
         Output ("Regression test failed", False);

   end Test_Regression;

   -------------------
   -- Test_Register --
   -------------------

   procedure Test_Register (How_Many : Natural);

   procedure Test_Register (How_Many : Natural)
   is
      Key_Root   : constant String := "Key";
      Value_Root : constant String := "Root";

      procedure Test_Lookup (How_Many : Natural);

      procedure Test_Lookup (How_Many : Natural)
      is
         Key_Root   : constant String := "Key";
         Value_Root : constant String := "Root";
      begin
         for J in 1 .. How_Many loop
            declare
               Count : constant String := Natural'Image (J);
               Key   : constant String := Key_Root
                 & Count (Count'First + 1 .. Count'Last);
               Content : constant String_Access
                 := My_Dict.Lookup (Key, Default => null);
               Value : constant String := Value_Root
                 & Count (Count'First + 1 .. Count'Last);
            begin
               if Content = null
                 or else Value /= Content.all then

                  Output ("Regression occured for key "
                          & Key
                          & " at stage #"
                          & Integer'Image (How_Many),
                          False);
                  raise Program_Error;
               end if;
            end;
         end loop;
      end Test_Lookup;

   begin
      for J in 1 .. How_Many loop
         declare
            Count : constant String := Natural'Image (J);
            Key   : constant String := Key_Root
              & Count (Count'First + 1 .. Count'Last);
            Value : constant String := Value_Root
              & Count (Count'First + 1 .. Count'Last);

         begin
            My_Dict.Register (Key, new String'(Value));
            Test_Lookup (J);
         end;
      end loop;
      Output ("Register", True);

   end Test_Register;

begin
   Output ("Initialization", True);
   Test_Register (500);
   Test_Regression;
   End_Report;

end Test000;
