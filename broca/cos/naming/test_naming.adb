with Ada.Text_IO;
with Ada.Exceptions;              use Ada.Exceptions;
with Menu;                        use Menu;

with CORBA;
with CORBA.Object;
with CORBA.ORB;

with CosNaming;                        use CosNaming;
with CosNaming.NamingContext;          use CosNaming.NamingContext;
with CosNaming.NamingContext.Helper;
with CosNaming.BindingIterator;        use CosNaming.BindingIterator;
with CosNaming.BindingIterator.Helper;
with CosNaming.NamingContext.Impl;

with File;                         use File;
with File.Impl;
with File.Helper;

with Broca.Basic_Startup;
pragma Elaborate (Broca.Basic_Startup);

procedure Test_Naming is

   package Names renames CosNaming.IDL_SEQUENCE_CosNaming_NameComponent;

   Null_Name    : constant Name          := Name    (Names.Null_Sequence);
   Null_Istring : constant Istring       := Istring (CORBA.Null_String);
   Null_NC      : constant NameComponent := (Null_Istring, Null_Istring);

   function From
     (S   : String_Access;
      Sep : Character := '/')
     return NamingContext.Ref;

   function Parent
     (S   : String_Access;
      Sep : Character := '/')
     return NamingContext.Ref;

   function To_Dir
     (S   : String_Access;
      Sep : Character := '/')
     return NamingContext.Ref;

   function To_File
     (S   : String_Access;
      Sep : Character := '/')
     return File.Ref;

   function To_Name
     (S   : String_Access;
      Sep : Character := '/')
      return Name;

   function To_String
     (N   : Names.Element_Array;
      Sep : Character := '/')
     return String;

   function To_String
     (N   : Name;
      Sep : Character := '/')
     return String;

   Argv    : String_Access;
   Argc    : Natural;
   Dir     : NamingContext.Ref;
   Obj     : File.Ref;
   CWD     : NamingContext.Ref;
   PWD     : Name := Null_Name;
   Root    : NamingContext.Ref;

   ------------
   -- Parent --
   ------------

   function Parent
     (S   : String_Access;
      Sep : Character := '/')
     return NamingContext.Ref
   is
      N : Name := To_Name (S, Sep);

   begin
      if Length (N) = 1 then
         return From (S);

      else
         return CosNaming.NamingContext.Helper.To_Ref
           (Resolve (From (S), Tail (N, 1, Null_NC)));
      end if;
   end Parent;

   ----------
   -- From --
   ----------

   function From
     (S   : in  String_Access;
      Sep : in  Character := '/')
     return NamingContext.Ref is
   begin
      if S (S'First) = Sep then
         return Root;
      else
         return CWD;
      end if;
   end From;

   ------------
   -- To_Dir --
   ------------

   function To_Dir
     (S   : in String_Access;
      Sep : in Character := '/')
     return NamingContext.Ref is
   begin
      return NamingContext.Helper.To_Ref (Resolve (From (S), To_Name (S)));
   exception
      when others =>
         Ada.Text_IO.Put_Line ("No such directory " & S.all);
         raise;
   end To_Dir;

   -------------
   -- To_File --
   -------------

   function To_File
     (S   : String_Access;
      Sep : Character := '/')
     return File.Ref is
   begin
      return File.Helper.To_Ref (Resolve (From (S), To_Name (S)));
   exception
      when others =>
         Ada.Text_IO.Put_Line ("No such file " & S.all);
         raise;
   end To_File;

   -------------
   -- To_Name --
   -------------

   function To_Name
     (S   : String_Access;
      Sep : Character := '/')
      return Name
   is
      Element : NameComponent;
      Result  : Name;
      First   : Natural;
      Last    : Natural;

   begin
      Element.Kind := Null_Istring;

      Last := S'First;
      while Last <= S'Last loop
         First := Last;
         while Last <= S'Last
           and then S (Last) /= Sep
         loop
            Last := Last + 1;
         end loop;

         Element.Id := To_CORBA_String (S (First .. Last - 1));
         Append (Result, Element);

         while Last <= S'Last
           and then S (Last) = Sep
         loop
            Last := Last + 1;
         end loop;
      end loop;

      return Result;
   end To_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (N   : Name;
      Sep : Character := '/')
     return String is
   begin
      return To_String (Names.To_Element_Array (Names.Sequence (N)), Sep);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (N   : Names.Element_Array;
      Sep : Character := '/')
     return String is
   begin
      if N'Length = 0 then
         return "";

      elsif N'Length = 1 then
         return To_Standard_String (N (N'First).Id);

      else
         return To_Standard_String (N (N'First).Id) & Sep &
           To_String (N (N'First + 1 .. N'Last), Sep);
      end if;
   end To_String;

   Back    : Name := To_Name (new String'(".."));
   Here    : Name := To_Name (new String'("."));

begin
   Broca.Basic_Startup.Initiate_Server;

   Ada.Text_IO.Put_Line ("create root directory");
   CWD := NamingContext.Impl.New_Context;
   Bind_Context (CWD, Here, CWD);
   Bind_Context (CWD, Back, CWD);

   loop
      Argc := Count;
      if Argc > 0 then
         begin
            Argv := Argument (1);
            To_Lower (Argv);
            if Argv.all = "exit" then
               exit;

            elsif Argc = 1
              and then Argv.all = "pwd"
            then
               Ada.Text_IO.Put_Line ('/' & To_String (PWD));

            elsif Argc = 2
              and then Argv.all = "chdir"
            then
               Argv := Argument (2);
               CWD  := To_Dir (Argv);

            elsif Argc = 2
              and then Argv.all = "rmkdir"
            then
               Argv  := Argument (2);
               Dir   := New_Context (From (Argv));
               Bind_Context (From (Argv), To_Name (Argv), Dir);
               Bind_Context (Dir, Here, Dir);
               Bind_Context (Dir, Back, Parent (Argv));

            elsif Argc = 2
              and then Argv.all = "lmkdir"
            then
               Argv  := Argument (2);
               Dir   := NamingContext.Impl.New_Context;
               Bind_Context (From (Argv), To_Name (Argv), Dir);
               Bind_Context (Dir, Here, Dir);
               Bind_Context (Dir, Back, Parent (Argv));

            elsif Argc = 3
              and then Argv.all = "write"
            then
               Argv  := Argument (2);
               begin
                  Obj := To_File (Argv);
               exception when others =>
                  Obj := File.Impl.New_File;
                  Bind (From (Argv), To_Name (Argv), CORBA.Object.Ref (Obj));
               end;
               Argv := Argument (3);
               Set_Image (Obj, CORBA.To_CORBA_String (Argv.all));

            elsif Argc = 2
              and then Argv.all = "read"
            then
               Argv  := Argument (2);
               Obj   := To_File (Argv);
               Ada.Text_IO.Put_Line
                 (CORBA.To_Standard_String (Get_Image (Obj)));

            elsif Argc < 3
              and then Argv.all = "list"
            then
               if Argc = 1 then
                  Dir   := CWD;
               else
                  Argv := Argument (2);
                  Dir  := To_Dir (Argv);
               end if;

               declare
                  Empty    : BindingList;
                  Iterator : BindingIterator.Ref;
                  Forward  : BindingIterator_Forward.Ref;
                  Done     : CORBA.Boolean;
                  Object   : Binding;
               begin
                  List (Dir, 0, Empty, Forward);
                  Iterator := Convert_Forward.To_Ref (Forward);

                  Done := True;
                  while Done loop
                     Next_One (Iterator, Object, Done);
                     Ada.Text_IO.Put (To_String (Object.Binding_Name));
                    if Object.Binding_Type = NContext then
                        Ada.Text_IO.Put ('/');
                     end if;
                     Ada.Text_IO.New_Line;
                  end loop;
               end;

            elsif Argc = 3
              and then Argv.all = "mount"
            then
               declare
                  Result : CORBA.Object.Ref;
                  Child  : NamingContext.Ref;
               begin
                  Argv   := Argument (3);
                  CORBA.ORB.String_To_Object
                    (CORBA.To_CORBA_String (Argv.all), Result);
                  Child  := NamingContext.Helper.To_Ref (Result);
                  Argv   := Argument (2);
                  Bind_Context (From (Argv), To_Name (Argv), Child);
               end;

            elsif Argc < 3
              and then Argv.all = "df"
            then
               if Argc = 1 then
                  Dir := CWD;
                  Ada.Text_IO.Put (To_String (PWD));

               else
                  Argv := Argument (2);
                  Dir  := To_Dir (Argv);
                  Ada.Text_IO.Put (Argv.all);
               end if;

               Ada.Text_IO.Put_Line
                 (ASCII.HT &
                  CORBA.To_Standard_String (Object_To_String (Dir)));

            else
               Ada.Text_IO.Put_Line ("syntax error");
            end if;

         exception
            when E : others =>
               Ada.Text_IO.Put_Line ("raise "& Exception_Name (E));
               Ada.Text_IO.Put_Line (Exception_Message (E));
         end;
      end if;
   end loop;

end Test_Naming;
