with Ada.Text_IO;
with Ada.Exceptions;              use Ada.Exceptions;
with Menu;                        use Menu;

with CORBA;
with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with PortableServer;

with CosNaming;                        use CosNaming;
with CosNaming.NamingContext;          use CosNaming.NamingContext;
with CosNaming.NamingContext.Helper;
with CosNaming.BindingIterator;        use CosNaming.BindingIterator;
with CosNaming.BindingIterator.Helper;
with CosNaming.NamingContext.Impl;

with File;                         use File;
with File.Impl;
with File.Helper;

with Broca.Server_Tools;
pragma Elaborate (Broca.Server_Tools);

procedure Test_Naming is

   package Names renames CosNaming.IDL_SEQUENCE_CosNaming_NameComponent;

   Null_Name    : constant Name          := Name    (Names.Null_Sequence);
   Null_Istring : constant Istring       := Istring (CORBA.Null_String);
   Null_NC      : constant NameComponent := (Null_Istring, Null_Istring);

   type Command is
     (Help,
      Quit,
      Pwd,
      Read,
      Write,
      List,
      Lmkdir,
      Rmkdir,
      Chdir,
      Rmdir,
      Mount,
      Df);

   Syntax_Error : exception;

   function M (S : String) return String_Access;
   function M (S : String) return String_Access is
   begin
      return new String'(S);
   end M;

   Help_Messages : constant array (Command) of String_Access
     := (Help   => M ("print this message"),
         Quit   => M ("quit this shell"),
         Pwd    => M ("print working directory"),
         Read   => M ("read <F>, read string from file F"),
         Write  => M ("write <F> <S>, write string S in file F"),
         List   => M ("list [<D>], list files in dir D [def = <.>]"),
         Lmkdir => M ("lmdir <D>, make local dir and bind it to <D>"),
         Rmkdir => M ("rmkdir <D>, make dir in parent dir and bind it to <D>"),
         Chdir  => M ("chdir <D>, change current dir to <D>"),
         Rmdir  => M ("rmdir <D>, remove dir <D>"),
         Mount  => M ("mount <D> <IOR>, bind dir <D> to a given dir <IOR>"),
         Df     => M ("df [<D>], print <IOR> of a given dir <D> [def = <.>]"));

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

   procedure Usage;

   Argv    : String_Access;
   Argc    : Natural;
   Dir     : NamingContext.Ref;
   Obj     : CORBA.Object.Ref;
   Fil     : File.Ref;
   WDR     : NamingContext.Ref;
   WDN     : Name := Null_Name;
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
           (Resolve (From (S), Name (Head (N, Length (N) - 1, Null_NC))));
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
         return WDR;
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
         return To_Standard_String (N (N'First).Id) & ASCII.HT &
                To_Standard_String (N (N'First).Kind);

      else
         return To_Standard_String (N (N'First).Id) & Sep &
           To_String (N (N'First + 1 .. N'Last), Sep);
      end if;
   end To_String;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      for I in Help_Messages'Range loop
         Ada.Text_IO.Put_Line (I'Img & Ascii.HT & Help_Messages (I).all);
      end loop;
      Ada.Text_IO.New_Line;
   end Usage;

   Back    : Name := To_Name (new String'(".."));
   Here    : Name := To_Name (new String'("."));
   Cmmd    : Command;

begin
   Broca.Server_Tools.Initiate_Server;

   Ada.Text_IO.Put_Line ("create root directory");
   Broca.Server_Tools.Servant_To_Reference
     (PortableServer.Servant (NamingContext.Impl.Create), WDR);

   Bind_Context (WDR, Here, WDR);
   Bind_Context (WDR, Back, WDR);

   loop
      Argc := Count;
      if Argc > 0 then
         begin
            Argv := Argument (1);
            begin
               Cmmd := Command'Value (Argv.all);
            exception when Constraint_Error =>
               raise Syntax_Error;
            end;
            case Cmmd is
               when Help =>
                  Usage;

               when Quit =>
                 exit;

               when Pwd =>
                  if Argc /= 1 then
                     raise Syntax_Error;
                  end if;
                  Ada.Text_IO.Put_Line ('/' & To_String (WDN));

               when Chdir =>
                  if Argc /= 2 then
                    raise Syntax_Error;
                  end if;
                  Argv := Argument (2);
                  WDR  := To_Dir (Argv);

               when Rmkdir =>
                  if Argc /= 2 then
                    raise Syntax_Error;
                  end if;
                  Argv  := Argument (2);
                  Dir   := New_Context (From (Argv));
                  Bind_Context (From (Argv), To_Name (Argv), Dir);
                  Bind_Context (Dir, Here, Dir);
                  Bind_Context (Dir, Back, Parent (Argv));

               when Lmkdir =>
                  if Argc /= 2 then
                     raise Syntax_Error;
                  end if;
                  Argv  := Argument (2);
                  Broca.Server_Tools.Servant_To_Reference
                    (PortableServer.Servant (NamingContext.Impl.Create), Dir);
                  Bind_Context (From (Argv), To_Name (Argv), Dir);
                  Bind_Context (Dir, Here, Dir);
                  Bind_Context (Dir, Back, Parent (Argv));

               when Write =>
                  if Argc /= 3 then
                    raise Syntax_Error;
                  end if;
                  Argv  := Argument (2);
                  begin
                     Fil := To_File (Argv);

                  exception when others =>
                     Ada.Text_IO.Put_Line ("Creating file " & Argv.all);
                     Fil := File.Impl.New_File;
                     Bind (From (Argv),
                           To_Name (Argv),
                           CORBA.Object.Ref (Fil));
                  end;
                  Argv := Argument (3);
                  Set_Image (Fil, CORBA.To_CORBA_String (Argv.all));

               when Read =>
                  if Argc /= 2 then
                     raise Syntax_Error;
                  end if;
                  Argv  := Argument (2);
                  Fil   := To_File (Argv);
                  Ada.Text_IO.Put_Line
                    (CORBA.To_Standard_String (Get_Image (Fil)));

               when List =>
                  if Argc >= 3 then
                     raise Syntax_Error;
                  end if;
                  if Argc = 1 then
                     Dir   := WDR;
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
                     Ada.Text_IO.New_Line;

                     Done := True;
                     while Done loop
                        Next_One (Iterator, Object, Done);
                        Ada.Text_IO.Put (To_String (Object.Binding_Name));
                        if Object.Binding_Type = NContext then
                           Ada.Text_IO.Put ('/');
                        end if;
                        Ada.Text_IO.New_Line;
                     end loop;

                     Destroy (Iterator);
                  end;

               when Mount =>
                  if Argc /= 3 then
                     raise Syntax_Error;
                  end if;
                  Argv := Argument (3);
                  CORBA.ORB.String_To_Object
                    (CORBA.To_CORBA_String (Argv.all), Obj);
                  Dir  := NamingContext.Helper.To_Ref (Obj);
                  Argv := Argument (2);
                  Bind_Context (From (Argv), To_Name (Argv), Dir);

               when Df =>

                  if Argc >= 3 then
                     raise Syntax_Error;
                  end if;
                  if Argc = 1 then
                     Dir := WDR;
                     Ada.Text_IO.Put (To_String (WDN));

                  else
                     Argv := Argument (2);
                     Dir  := To_Dir (Argv);
                     Ada.Text_IO.Put (Argv.all);
                  end if;

                  Ada.Text_IO.Put_Line
                    (ASCII.HT &
                     CORBA.To_Standard_String (Object_To_String (Dir)));

               when Rmdir =>
                  if Argc /= 2 then
                     raise Syntax_Error;
                  end if;
                  Argv := Argument (2);
                  Obj := Resolve (From (Argv), To_Name (Argv));
                  Dir := NamingContext.Helper.To_Ref (Obj);
                  declare
                     Bindings : BindingList;
                     Forward  : BindingIterator_Forward.Ref;

                  begin
                     List (Dir, 3, Bindings, Forward);
                     if Length (Bindings) /= 2 then
                        Ada.Text_IO.Put_Line ("directory not empty");

                     else
                        Unbind (From (Argv), To_Name (Argv) & Here);
                        Unbind (From (Argv), To_Name (Argv) & Back);
                        Unbind (From (Argv), To_Name (Argv));
                        Destroy (Dir);
                     end if;
                  end;

            end case;

         exception
            when Syntax_Error =>
               Ada.Text_IO.Put_Line ("syntax error");

            when E : others =>
               Ada.Text_IO.Put_Line ("raised "& Exception_Information (E));
               Ada.Text_IO.Put_Line (Exception_Message (E));
         end;
      end if;
   end loop;

end Test_Naming;
