------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
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

with PolyORB.Dynamic_Dict;
with PolyORB.Utils.Report;
with PolyORB.Utils.Strings;

procedure Test000 is

   use PolyORB.Utils.Report;
   use PolyORB.Utils.Strings;

   package My_Dict is new PolyORB.Dynamic_Dict (Value => String_Ptr);

   ---------------------
   -- Test_Regression --
   ---------------------

   procedure Test_Regression;
   --  This particular configuration raised an exception.

   procedure Test_Regression
   is

      Values : constant array (Positive range <>) of String_Ptr :=
        (+"tasking.profiles.full_tasking.threads",
         +"tasking.threads",
         +"tasking.profiles.full_tasking.mutexes",
         +"tasking.mutexes",
         +"tasking.profiles.full_tasking.condition_variables",
         +"tasking.condition_variables",
         +"exceptions.stack",
         +"exceptions",
         +"smart_pointers",
         +"binding_data.iiop",
         +"protocols.giop",
         +"orb.thread_pool",
         +"orb.tasking_policy",
         +"orb.threads_init",
         +"orb.tasking_policy_init",
         +"orb",
         +"corba.orb",
         +"corba.initial_references",
         +"tcp_access_points.corba",
         +"tcp_access_points.srp",
         +"tasking.soft_links",
         +"soft_links");

      Result : String_Ptr;
   begin
      for J in Values'Range loop
         My_Dict.Register (Values (J).all, +"foo");

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

      Output ("Check no exception is raised", True);
   exception
      when others =>
         Output ("Check no exception is raised", False);

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
               Content : constant String_Ptr
                 := My_Dict.Lookup (Key, Default => null);
               Value : constant String := Value_Root
                 & Count (Count'First + 1 .. Count'Last);
            begin
               if Content = null or else Value /= Content.all then
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
            My_Dict.Register (Key, +Value);
            Test_Lookup (J);
         end;
      end loop;
      Output ("Register", True);

   end Test_Register;

   -------------------
   -- Test_For_Each --
   -------------------

   procedure Test_For_Each;
   --  Test for generic key/value associations iterator

   subtype Indices is Integer range 1 .. 64;
   type Bool_Arr is array (Indices) of Boolean;
   Seen : Bool_Arr := (others => False);

   function Value_For (J : Indices) return String;
   function Value_For (J : Indices) return String is
   begin
      return "-->" & J'Img & "<--";
   end Value_For;

   procedure Check_Association (K : String; V : String_Ptr);
   procedure Check_Association (K : String; V : String_Ptr) is
      J : Indices;
   begin
      J := Indices'Value (K);
      if V.all /= Value_For (J) then
         Output ("Invalid association:" & K & " => " & V.all, False);
      elsif Seen (J) then
         Output ("Key" & K & "already seen", False);
      else
         Seen (J) := True;
      end if;
   exception
      when others =>
         Output ("invalid key: " & K, False);
   end Check_Association;

   procedure Test_For_Each is
      Val : array (Indices) of String_Ptr;

   begin
      My_Dict.Reset;
      for J in Val'Range loop
         My_Dict.Register (J'Img, +Value_For (J));
      end loop;
      My_Dict.For_Each (Check_Association'Access);
      Output ("test For_Each", Seen = Bool_Arr'(others => True));
   end Test_For_Each;

begin
   Output ("Initialization", True);
   Test_Register (500);
   Test_Regression;
   Test_For_Each;
   End_Report;

end Test000;
