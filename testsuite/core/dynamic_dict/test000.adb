with PolyORB.Dynamic_Dict;

with PolyORB.Report;

procedure Test000 is

   type String_Access is access all String;

   package My_Dict is new PolyORB.Dynamic_Dict
     (Value => String_Access, No_Value => null);

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

               PolyORB.Report.Output ("Regression occured for key "
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

      PolyORB.Report.Output ("Regression did not occured", True);
   exception
      when others =>
         PolyORB.Report.Output ("Regression test failed", False);

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

                  PolyORB.Report.Output ("Regression occured for key "
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
      PolyORB.Report.Output ("Register", True);

   end Test_Register;

begin
   PolyORB.Report.Output ("Initialization", True);
   Test_Register (500);
   Test_Regression;
   PolyORB.Report.End_Report;

end Test000;
