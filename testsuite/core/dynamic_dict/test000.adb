with PolyORB.Dynamic_Dict;
with Report;

procedure Test000 is

   type String_Access is access all String;

   package My_Dict is new PolyORB.Dynamic_Dict
     (Value => String_Access, No_Value => null);

   ---------------------
   -- Test_Regression --
   ---------------------

   procedure Test_Regression;
   --  This particular configuration raises an exception.

   procedure Test_Regression
   is
      package My_Dict2 is new PolyORB.Dynamic_Dict
        (Value => String_Access, No_Value => null);

   begin
      My_Dict2.Register ("tasking.profiles.full_tasking.threads",
                         new String'("foo"));
      My_Dict2.Register ("tasking.threads",
                         new String'("foo"));
      My_Dict2.Register ("tasking.profiles.full_tasking.mutexes",
                         new String'("foo"));
      My_Dict2.Register ("tasking.mutexes",
                         new String'("foo"));
      My_Dict2.Register ("tasking.profiles.full_tasking.condition_variables",
                         new String'("foo"));
      My_Dict2.Register ("tasking.condition_variables",
                         new String'("foo"));
      My_Dict2.Register ("exceptions.stack",
                         new String'("foo"));
      My_Dict2.Register ("exceptions",
                         new String'("foo"));
      My_Dict2.Register ("smart_pointers",
                         new String'("foo"));
      My_Dict2.Register ("binding_data.iiop",
                         new String'("foo"));
      My_Dict2.Register ("protocols.giop",
                         new String'("foo"));
      My_Dict2.Register ("orb.thread_pool",
                         new String'("foo"));
      My_Dict2.Register ("orb.tasking_policy",
                         new String'("foo"));
      My_Dict2.Register ("orb.threads_init",
                         new String'("foo"));
      My_Dict2.Register ("orb.tasking_policy_init",
                         new String'("foo"));
      My_Dict2.Register ("orb",
                         new String'("foo"));
      My_Dict2.Register ("corba.orb",
                         new String'("foo"));
      My_Dict2.Register ("corba.initial_references",
                         new String'("foo"));
      My_Dict2.Register ("tcp_access_points.corba",
                         new String'("foo"));
      My_Dict2.Register ("tcp_access_points.srp",
                         new String'("foo"));
      My_Dict2.Register ("tasking.soft_links",
                         new String'("foo"));
      My_Dict2.Register ("soft_links",
                         new String'("foo"));
      declare
         Content : constant String_Access
           := My_Dict2.Lookup ("protocols.giop");
      begin
         null;
      end;

      Report.Output ("Regression occured", False);
   exception
      when Constraint_Error =>
         Report.Output ("Regression occured", True);

      when others =>
         Report.Output ("Regression occured", False);
   end Test_Regression;

   -------------------
   -- Test_Register --
   -------------------

   procedure Test_Register (How_Many : Natural := 10);

   procedure Test_Register (How_Many : Natural := 10)
   is
      Key_Root   : constant String := "Key";
      Value_Root : constant String := "Root";

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
         end;
      end loop;
      Report.Output ("Register", True);

   end Test_Register;

   -------------------
   -- Test_Lookup --
   -------------------

   procedure Test_Lookup (How_Many : Natural := 10);

   procedure Test_Lookup (How_Many : Natural := 10)
   is
      Key_Root   : constant String := "Key";
      Value_Root : constant String := "Root";
      Result     : Boolean := True;
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
            Result := Result and Value = Content.all;
         end;
      end loop;
      Report.Output ("Lookup", Result);
   end Test_Lookup;

begin
   Test_Register (1000);
   Test_Lookup   (1000);
   Test_Regression;
   Report.End_Report;


end Test000;
