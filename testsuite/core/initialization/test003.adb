with Ada.Text_IO;

with PolyORB.Initialization;
with PolyORB.Utils.Report;
with PolyORB.Utils.Strings;

procedure Test003 is

   use Ada.Text_IO;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Report;
   use PolyORB.Utils.Strings;

   generic
      Name : String;
   procedure Init;

   procedure Init is
   begin
      Put_Line ("Initializing module " & Name);
   end Init;

   procedure Init_Foo is new Init ("foo");
   procedure Init_Bar is new Init ("bar");

   Empty_List : String_Lists.List;

begin
   Register_Module
     (Module_Info'
      (Name => +"bar",
       Depends => Empty_List & "foo",
       Conflicts => Empty_List,
       Provides => Empty_List,
       Init => Init_Bar'Unrestricted_Access));

   Register_Module
     (Module_Info'
      (Name => +"foo",
       Depends => Empty_List & "bar",
       Conflicts => Empty_List,
       Provides => Empty_List,
       Init => Init_Foo'Unrestricted_Access));

   Initialize_World;

   Output ("Test initialization #3", False);

exception
   when PolyORB.Initialization.Circular_Dependency =>
      Output ("Test initialization #3", True);
      End_Report;

   when others =>
      Output ("Test initialization #3", False);

end Test003;
