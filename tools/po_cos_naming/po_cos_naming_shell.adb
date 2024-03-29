------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O _ C O S _ N A M I N G _ S H E L L                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2023, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;              use Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Command_Line;           use GNAT.Command_Line;

with Menu;                        use Menu;

with CORBA;
with CORBA.Object;
with CORBA.ORB;

with PortableServer;

with CosNaming;                        use CosNaming;
with CosNaming.NamingContext;          use CosNaming.NamingContext;
with CosNaming.NamingContext.Helper;
with CosNaming.BindingIterator;        use CosNaming.BindingIterator;
with CosNaming.NamingContext.Impl;

with File;                         use File;
with File.Impl;
with File.Helper;

with PolyORB.CORBA_P.Naming_Tools; use PolyORB.CORBA_P.Naming_Tools;
with PolyORB.CORBA_P.Server_Tools;

with PolyORB.Setup.Thread_Pool_Server;
pragma Elaborate_All (PolyORB.Setup.Thread_Pool_Server);
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

procedure PO_COS_Naming_Shell is

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
      Bind,
      Df);

   Syntax_Error : exception;

   -------
   -- M --
   -------

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
         Mount  => M ("mount <D> <IOR>, bind dir name <D> to dir <IOR>"),
         Bind   => M ("bind <N> <IOR>, bind object name <N> to object <IOR>"),
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
   --  Print help on console's functions

   procedure Cmd_Line_Usage;
   --  Print help on command line's parameters

   Argv    : String_Access;
   Argc    : Natural;
   Dir     : NamingContext.Ref;
   Obj     : CORBA.Object.Ref;
   Fil     : File.Ref;
   WDR     : NamingContext.Ref;
   WDN     : constant Name := Null_Name;
   Root    : NamingContext.Ref;

   ------------
   -- Parent --
   ------------

   function Parent
     (S   : String;
      Sep : Character := '/') return NamingContext.Ref
   is
      N : constant Name := To_Name (S, Sep);
   begin
      if Length (N) = 1 then
         return From (S, Sep);
      else
         return CosNaming.NamingContext.Helper.To_Ref
           (resolve (From (S, Sep), Name'(Head (N, Length (N) - 1, Null_NC))));
      end if;
   end Parent;

   ----------
   -- From --
   ----------

   function From
     (S   : String;
      Sep : Character := '/') return NamingContext.Ref is
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
     (S   : String;
      Sep : Character := '/') return NamingContext.Ref is
   begin
      return NamingContext.Helper.To_Ref
        (resolve (From (S, Sep), To_Name (S, Sep)));
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
      return resolve (From (S, Sep), To_Name (S, Sep));
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
     renames PolyORB.CORBA_P.Naming_Tools.Parse_Name;

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
         return To_Standard_String (N (N'First).id) & ASCII.HT &
                To_Standard_String (N (N'First).kind);

      else
         return To_Standard_String (N (N'First).id) & Sep &
           To_String (N (N'First + 1 .. N'Last), Sep);
      end if;
   end To_String;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      for I in Help_Messages'Range loop
         Ada.Text_IO.Put_Line (I'Img & ASCII.HT & Help_Messages (I).all);
      end loop;
      Ada.Text_IO.New_Line;
   end Usage;

   Back     : constant Name := To_Name ("...subcontext");
   Here     : constant Name := To_Name ("..subcontext");
   Cmmd     : Command;
   Register_Service : Boolean := False;

   ---------------
   -- Bind_Self --
   ---------------

   procedure Bind_Self (Self : CosNaming.NamingContext.Ref; As : Name);

   procedure Bind_Self (Self : CosNaming.NamingContext.Ref; As : Name) is
   begin
      bind_context (Self, As, Self);
   exception
      when E : others =>
         Ada.Text_IO.Put ("Warning: could not bind " & To_String (As) & ": ");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
   end Bind_Self;

   --------------------
   -- Cmd_Line_Usage --
   --------------------

   procedure Cmd_Line_Usage is
   begin
      Ada.Text_IO.Put_Line ("po_cos_naming_shell [-s] [-i] [-I <IOR>] [-n]");
      Ada.Text_IO.Put_Line (" -s register root directory initial reference");
      Ada.Text_IO.Put_Line (" -i retrieve root directory initial reference");
      Ada.Text_IO.Put_Line (" -I <IOR>, use object denoted by IOR as"
                            & " root directory");
      Ada.Text_IO.Put_Line (" -n retrieve root directory by name");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Note: if no Root is provided, create a new one");
   end Cmd_Line_Usage;

   --  Start of processing for Test_Naming_CORBA_I

begin
   CORBA.ORB.Initialize ("ORB");
   PolyORB.CORBA_P.Server_Tools.Initiate_Server (Start_New_Task => True);

   --  Parse the command line

   begin
      Initialize_Option_Scan ('-', False, "");

      loop
         case Getopt ("i n s I:") is
            when ASCII.NUL =>
               exit;

            when 's' =>
               Register_Service := True;

            when 'i' =>
               begin
                  Ada.Text_IO.Put
                    ("retrieving root directory initial reference...");
                  Ada.Text_IO.Flush;
                  if not Is_Nil (WDR) then
                     raise Program_Error;
                  end if;
                  WDR := NamingContext.Helper.To_Ref
                           (CORBA.ORB.Resolve_Initial_References
                            (CORBA.ORB.ObjectId
                             (CORBA.String'
                              (CORBA.To_CORBA_String
                               ("NamingService")))));
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
               --  This never happens

               raise Program_Error;
         end case;
      end loop;

   exception
      when Invalid_Switch    =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Current_Error, "Invalid Switch " & Full_Switch);
         Cmd_Line_Usage;
         return;

      when Invalid_Parameter =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Current_Error, "No parameter for " & Full_Switch);
         Cmd_Line_Usage;
         return;
   end;

   --  If no root naming context is set up, create one

   if Is_Nil (WDR) then
      Ada.Text_IO.Put_Line ("creating root directory");
      PolyORB.CORBA_P.Server_Tools.Servant_To_Reference
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

   --  Console main loop: read inputs and process them

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
                  Dir   := NamingContext.Ref (new_context (From (Argv.all)));
                  bind_context (From (Argv.all), To_Name (Argv.all), Dir);
                  bind_context (Dir, Here, Dir);
                  bind_context (Dir, Back, Parent (Argv.all));

               when Lmkdir | Mkdir | Md =>
                  if Argc /= 2 then
                     raise Syntax_Error;
                  end if;
                  Argv  := Argument (2);
                  PolyORB.CORBA_P.Server_Tools.Servant_To_Reference
                    (PortableServer.Servant (NamingContext.Impl.Create), Dir);
                  bind_context (From (Argv.all), To_Name (Argv.all), Dir);
                  bind_context (Dir, Here, Dir);
                  bind_context (Dir, Back, Parent (Argv.all));

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
                     bind
                       (From (Argv.all), To_Name (Argv.all),
                        CORBA.Object.Ref (Fil));
                  end;
                  Argv := Argument (3);
                  set_Image (Fil, CORBA.To_CORBA_String (Argv.all));

               when Read =>
                  if Argc /= 2 then
                     raise Syntax_Error;
                  end if;
                  Argv  := Argument (2);
                  Fil   := To_File (Argv.all);
                  Ada.Text_IO.Put_Line
                    (CORBA.To_Standard_String (get_Image (Fil)));

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
                     list (Dir, 0, Empty, Forward);
                     Iterator := Convert_Forward.To_Ref (Forward);
                     Ada.Text_IO.New_Line;

                     Done := True;
                     while Done loop
                        next_one (Iterator, Object, Done);
                        Ada.Text_IO.Put (To_String (Object.binding_name));
                        if Object.binding_type = ncontext then
                           Ada.Text_IO.Put ('/');
                        end if;
                        Ada.Text_IO.New_Line;
                     end loop;

                     destroy (Iterator);
                  end;

               when Mount | Bind =>
                  if Argc /= 3 then
                     raise Syntax_Error;
                  end if;
                  Argv := Argument (3);
                  CORBA.ORB.String_To_Object
                    (CORBA.To_CORBA_String (Argv.all), Obj);
                  Argv := Argument (2);
                  if Cmmd = Mount then
                     Dir  := NamingContext.Helper.To_Ref (Obj);
                     bind_context (From (Argv.all), To_Name (Argv.all), Dir);
                  else
                     bind (From (Argv.all), To_Name (Argv.all), Obj);
                  end if;

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
                     CORBA.To_Standard_String
                     (CORBA.ORB.Object_To_String (Dir)));

               when Rmdir =>
                  if Argc /= 2 then
                     raise Syntax_Error;
                  end if;
                  Argv := Argument (2);
                  Obj := resolve (From (Argv.all), To_Name (Argv.all));
                  Dir := NamingContext.Helper.To_Ref (Obj);
                  declare
                     Bindings : BindingList;
                     Forward  : BindingIterator_Forward.Ref;

                  begin
                     list (Dir, 3, Bindings, Forward);
                     if Length (Bindings) /= 2 then
                        Ada.Text_IO.Put_Line ("directory not empty");

                     else
                        unbind (From (Argv.all), To_Name (Argv.all) & Here);
                        unbind (From (Argv.all), To_Name (Argv.all) & Back);
                        unbind (From (Argv.all), To_Name (Argv.all));
                        destroy (Dir);
                     end if;
                  end;

            end case;

         exception
            when Syntax_Error =>
               Ada.Text_IO.Put_Line ("syntax error");

            when E : others =>
               Ada.Text_IO.Put_Line ("raised " & Exception_Information (E));
               Ada.Text_IO.Put_Line (Exception_Message (E));
         end;
      end if;
   end loop;

end PO_COS_Naming_Shell;
