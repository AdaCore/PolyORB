------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--                          T E S T _ N A M I N G                           --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;              use Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with GNAT.Command_Line;           use GNAT.Command_Line;
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

with Broca.Naming_Tools;                 use Broca.Naming_Tools;
with Broca.Server_Tools;

procedure Test_Naming is

   package Names renames CosNaming.IDL_SEQUENCE_CosNaming_NameComponent;

   Null_Name    : constant Name          := Name    (Names.Null_Sequence);
   Null_Istring : constant Istring       := Istring (CORBA.Null_String);
   Null_NC      : constant NameComponent := (Null_Istring, Null_Istring);

   Test_Name    : constant String        := "test_naming";

   type Command is
     (Help,
      Quit,
      Pwd,
      Read,
      Write,
      List,
      Ls,
      Namei,
      Lmkdir,
      Mkdir,
      Md,
      Rmkdir,
      Chdir,
      Cd,
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
         Ls     => M ("   (alias for LIST)"),
         Namei  => M ("namei <N>, show IOR bound to <N>"),
         Lmkdir => M ("lmdir <D>, make local dir and bind it to <D>"),
         Mkdir  => M ("   (alias for LMKDIR)"),
         Md     => M ("   (alias for LMKDIR)"),
         Rmkdir => M ("rmkdir <D>, make dir in parent dir and bind it to <D>"),
         Chdir  => M ("chdir <D>, change current dir to <D>"),
         Cd     => M ("   (alias for CHDIR)"),
         Rmdir  => M ("rmdir <D>, remove dir <D>"),
         Mount  => M ("mount <D> <IOR>, bind dir <D> to a given dir <IOR>"),
         Df     => M ("df [<D>], print <IOR> of a given dir <D> [def = <.>]"));

   function From
     (S   : String;
      Sep : Character := '/')
     return NamingContext.Ref;

   function Parent
     (S   : String;
      Sep : Character := '/')
     return NamingContext.Ref;

   function To_Dir
     (S   : String;
      Sep : Character := '/')
     return NamingContext.Ref;

   function To_File
     (S   : String;
      Sep : Character := '/')
     return File.Ref;

   function To_Object
     (S   : String;
      Sep : Character := '/')
     return CORBA.Object.Ref;

   function To_Name
     (S   : String;
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
     (S   : String;
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
     (S   : in  String;
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
     (S   : in String;
      Sep : in Character := '/')
     return NamingContext.Ref is
   begin
      return NamingContext.Helper.To_Ref (Resolve (From (S), To_Name (S)));
   exception
      when others =>
         Ada.Text_IO.Put_Line ("No such directory " & S);
         raise;
   end To_Dir;

   -------------
   -- To_File --
   -------------

   function To_File
     (S   : String;
      Sep : Character := '/')
     return File.Ref is
   begin
      return File.Helper.To_Ref (To_Object (S, Sep));
   end To_File;

   ---------------
   -- To_Object --
   ---------------

   function To_Object
     (S   : String;
      Sep : Character := '/')
     return CORBA.Object.Ref is
   begin
      return Resolve (From (S), To_Name (S));
   exception
      when others =>
         Ada.Text_IO.Put_Line ("No such object " & S);
         raise;
   end To_Object;

   -------------
   -- To_Name --
   -------------

   function To_Name
     (S   : String;
      Sep : Character := '/')
     return Name
     renames Broca.Naming_Tools.Parse_Name;

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

   Back     : Name := To_Name ("...subcontext");
   Here     : Name := To_Name ("..subcontext");
   Cmmd     : Command;
   Register_Service : Boolean := False;

   procedure Bind_Self
      (Self : CosNaming.NamingContext.Ref;
       As   : Name) is
   begin
      Bind_Context (Self, As, Self);
   exception
      when E : others =>
         Ada.Text_IO.Put ("Warning: could not bind " & To_String (As) & ": ");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
   end Bind_Self;

begin
   Broca.Server_Tools.Initiate_Server;
   begin
      Initialize_Option_Scan ('-', False, "");

      loop
         case Getopt ("i n s I:") is
            when ASCII.Nul =>
               exit;

            when 's' =>
               Register_Service := True;

            when 'i' =>
               begin
                  Ada.Text_IO.Put ("retrieving root directory initial reference...");
                  Ada.Text_IO.Flush;
                  if not Is_Nil (WDR) then
                     raise Program_Error;
                  end if;
                  WDR := NamingContext.Helper.To_Ref
                    (CORBA.ORB.Resolve_Initial_References
                     (CORBA.ORB.To_CORBA_String ("NamingService")));
                  Ada.Text_IO.Put_Line (" done");
               exception
                  when others =>
                     Ada.Text_IO.Put_Line ("error");
                     raise;
               end;

            when 'I' =>
               begin
                  Ada.Text_IO.Put ("locating main service by IOR...");
                  Ada.Text_IO.Flush;
                  if not Is_Nil (WDR) then
                     raise Program_Error;
                  end if;
                  CORBA.ORB.String_To_Object
                    (CORBA.To_CORBA_String (Parameter), WDR);
                  Ada.Text_IO.Put_Line (" done");
               exception
                  when others =>
                     Ada.Text_IO.Put_Line (" error");
                     raise;
               end;

            when 'n' =>
               begin
                  Ada.Text_IO.Put ("locating main service by name...");
                  Ada.Text_IO.Flush;
                  if not Is_Nil (WDR) then
                     raise Program_Error;
                  end if;

                  WDR := NamingContext.Helper.To_Ref (Locate (Test_Name));
                  Ada.Text_IO.Put_Line (" done");
               exception
                  when others =>
                     Ada.Text_IO.Put_Line (" error");
                     raise;
               end;

            when others =>
               --  This never happens.
               raise Program_Error;
         end case;
      end loop;

   exception
      when Invalid_Switch    =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Current_Error, "Invalid Switch " & Full_Switch);
         Usage;
      when Invalid_Parameter =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Current_Error, "No parameter for " & Full_Switch);
         Usage;
   end;

   if Is_Nil (WDR) then
      Ada.Text_IO.Put_Line ("creating root directory");
      Broca.Server_Tools.Servant_To_Reference
        (PortableServer.Servant (NamingContext.Impl.Create), WDR);

      if Register_Service then
         Ada.Text_IO.Put ("registering main service by name...");
         Ada.Text_IO.Flush;
         Register (Test_Name, CORBA.Object.Ref (WDR), Rebind => True);
         Ada.Text_IO.Put_Line (" done");
      end if;
   end if;

   Bind_Self (WDR, Here);
   Bind_Self (WDR, Back);

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

               when Chdir | Cd =>
                  if Argc /= 2 then
                    raise Syntax_Error;
                  end if;
                  Argv := Argument (2);
                  WDR  := To_Dir (Argv.all);

               when Rmkdir =>
                  if Argc /= 2 then
                    raise Syntax_Error;
                  end if;
                  Argv  := Argument (2);
                  Dir   := New_Context (From (Argv.all));
                  Bind_Context (From (Argv.all), To_Name (Argv.all), Dir);
                  Bind_Context (Dir, Here, Dir);
                  Bind_Context (Dir, Back, Parent (Argv.all));

               when Lmkdir | Mkdir | Md =>
                  if Argc /= 2 then
                     raise Syntax_Error;
                  end if;
                  Argv  := Argument (2);
                  Broca.Server_Tools.Servant_To_Reference
                    (PortableServer.Servant (NamingContext.Impl.Create), Dir);
                  Bind_Context (From (Argv.all), To_Name (Argv.all), Dir);
                  Bind_Context (Dir, Here, Dir);
                  Bind_Context (Dir, Back, Parent (Argv.all));

               when Write =>
                  if Argc /= 3 then
                    raise Syntax_Error;
                  end if;
                  Argv  := Argument (2);
                  begin
                     Fil := To_File (Argv.all);

                  exception when others =>
                     Ada.Text_IO.Put_Line ("Creating file " & Argv.all);
                     Fil := File.Impl.New_File;
                     Bind (From (Argv.all), To_Name (Argv.all),
                           CORBA.Object.Ref (Fil));
                  end;
                  Argv := Argument (3);
                  Set_Image (Fil, CORBA.To_CORBA_String (Argv.all));

               when Read =>
                  if Argc /= 2 then
                     raise Syntax_Error;
                  end if;
                  Argv  := Argument (2);
                  Fil   := To_File (Argv.all);
                  Ada.Text_IO.Put_Line
                    (CORBA.To_Standard_String (Get_Image (Fil)));

               when Namei =>
                  if Argc /= 2 then
                     raise Syntax_Error;
                  end if;

                  Argv := Argument (2);
                  Ada.Text_IO.Put_Line
                    (CORBA.To_Standard_String
                     (CORBA.Object.Object_To_String (To_Object (Argv.all))));

               when List | Ls =>
                  if Argc >= 3 then
                     raise Syntax_Error;
                  end if;
                  if Argc = 1 then
                     Dir   := WDR;
                  else
                     Argv := Argument (2);
                     Dir  := To_Dir (Argv.all);
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
                  Bind_Context (From (Argv.all), To_Name (Argv.all), Dir);

               when Df =>

                  if Argc >= 3 then
                     raise Syntax_Error;
                  end if;
                  if Argc = 1 then
                     Dir := WDR;
                     Ada.Text_IO.Put (To_String (WDN));

                  else
                     Argv := Argument (2);
                     Dir  := To_Dir (Argv.all);
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
                  Obj := Resolve (From (Argv.all), To_Name (Argv.all));
                  Dir := NamingContext.Helper.To_Ref (Obj);
                  declare
                     Bindings : BindingList;
                     Forward  : BindingIterator_Forward.Ref;

                  begin
                     List (Dir, 3, Bindings, Forward);
                     if Length (Bindings) /= 2 then
                        Ada.Text_IO.Put_Line ("directory not empty");

                     else
                        Unbind (From (Argv.all), To_Name (Argv.all) & Here);
                        Unbind (From (Argv.all), To_Name (Argv.all) & Back);
                        Unbind (From (Argv.all), To_Name (Argv.all));
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
