------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                            N A M I N G _ S H                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Exceptions;    use Ada.Exceptions;
with Menu;              use Menu;
with Naming_Files;      use Naming_Files;
with GLADE.Objects;     use GLADE.Objects;
with GLADE.Naming;      use GLADE.Naming;
with GLADE.Naming.Root;

procedure Naming_Sh is

   function PWD
     (NC : Naming_Context_Ref)
      return String;

   function To_Name
     (S   : String;
      Sep : Character := '/')
      return Name;

   function To_String
     (N   : Name_Component_Sequence;
      Sep : Character := '/')
     return String;

   function To_String
     (N   : Name;
      Sep : Character := '/')
     return String;

   Word : String_Access;
   Argc : Natural;
   Dir  : Naming_Context_Ref;
   Obj  : Object_Ref;
   File : Name;
   CWD  : Naming_Context_Ref := Root.Ref;

   -------------
   -- To_Name --
   -------------

   function To_Name
     (S   : String;
      Sep : Character := '/')
      return Name
   is
      Length : Natural := 1;
      Result : Name;
      First  : Natural;
      Last   : Natural;
   begin

      --  Count component in resulting sequence

      First := S'First;
      while First <= S'Last loop

         --  Skip non-separators

         while First <= S'Last
           and then S (First) /= Sep
         loop
            First := First + 1;
         end loop;

         exit when First > S'Last;
         Length := Length + 1;

         --  Skip separators

         while First <= S'Last
           and then S (First) = Sep
         loop
            First := First + 1;
         end loop;
      end loop;

      declare
         Sequence : Name_Component_Sequence (1 .. Length);
      begin
         Last := S'First;
         for I in Sequence'Range loop
            First := Last;
            while Last <= S'Last
              and then S (Last) /= Sep
            loop
               Last := Last + 1;
            end loop;

            Set_Istring (Sequence (I).Id, S (First .. Last - 1));

            while Last <= S'Last
              and then S (Last) = Sep
            loop
               Last := Last + 1;
            end loop;
         end loop;
         Set_Name (Result, Sequence);
         for I in Sequence'Range loop
            Set_Istring (Sequence (I).Id, No_String);
         end loop;
      end;

      return Result;
   end To_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (N   : Name;
      Sep : Character := '/')
      return String
   is
      Seq : Name_Component_Sequence := Get_Name (N);
   begin
      if Seq'Length = 0 then
         return "";

      else
         return To_String (Seq, Sep);
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (N   : Name_Component_Sequence;
      Sep : Character := '/')
     return String
   is
   begin
      if N'Length = 1 then
         return Get_Istring (N (N'First).Id);

      else
         return Get_Istring (N (N'First).Id) & Sep &
           To_String (N (N'First + 1 .. N'Last), Sep);
      end if;
   end To_String;

   Back : Name := To_Name ("..");
   Here : Name := To_Name (".");

   ---------
   -- PWD --
   ---------

   function PWD
     (NC : Naming_Context_Ref)
      return String
   is
      Prev : Naming_Context_Ref;
   begin
      Prev := Resolve (NC, Back);
      if Prev = NC then
         return "";
      else
         return PWD (Prev) & "/" & Get_Image (NC);
      end if;
   end PWD;

begin
   loop
      Set_Name (File, No_Name_Component_Sequence);
      Argc := Count;
      if Argc > 0 then
         begin
            Word := Argument (1);
            To_Lower (Word);
            if Word.all = "exit" then
               exit;

            elsif Argc = 1
              and then Word.all = "pwd"
            then
               Ada.Text_IO.Put_Line (PWD (CWD));

            elsif Argc = 2
              and then Word.all = "chdir"
            then
               Word := Argument (2);
               File := To_Name (Word.all);
               CWD  := Resolve (CWD, File);

            elsif Argc = 2
              and then Word.all = "rmkdir"
            then
               Word := Argument (2);
               File := To_Name (Word.all);
               Dir  := New_Context (CWD);
               Bind_Context (CWD, File, Dir);
               Set_Image (Dir, Word.all);
               Bind_Context (Dir, Here, Dir);
               Bind_Context (Dir, Back, CWD);

            elsif Argc = 2
              and then Word.all = "lmkdir"
            then
               Word := Argument (2);
               File := To_Name (Word.all);
               Dir  := New_Context;
               Bind_Context (CWD, File, Dir);
               Set_Image (Dir, Word.all);
               Bind_Context (Dir, Here, Dir);
               Bind_Context (Dir, Back, CWD);

            elsif Argc = 3
              and then Word.all = "write"
            then
               Word := Argument (2);
               File := To_Name (Word.all);
               begin
                  Obj := Resolve (CWD, File);
               exception when others =>
                  Obj := New_File;
                  Bind (CWD, File, Obj);
               end;
               Word := Argument (3);
               Set_Image (Obj, Word.all);

            elsif Argc = 2
              and then Word.all = "read"
            then
               Word := Argument (2);
               File := To_Name (Word.all);
               Obj  := Resolve (CWD, File);
               Ada.Text_IO.Put_Line (Get_Image (Obj));

            elsif Argc < 3
              and then Word.all = "list"
            then
               if Argc = 1 then
                  File := To_Name (".");
               else
                  Word := Argument (2);
                  File := To_Name (Word.all);
               end if;
               Dir  := Resolve (CWD, File);
               declare
                  Empty    : Binding_List;
                  Iterator : Binding_Iterator_Ref;
                  Done     : Boolean;
                  Object   : Binding;
               begin
                  List (Dir, 0, Empty, Iterator);
                  if Iterator /= null then
                     Done := True;
                     while Done loop
                        Next_One (Iterator, Object, Done);
                        Ada.Text_IO.Put (To_String (Object.BN));
                        if Object.BT = Naming_Context_Type then
                           Ada.Text_IO.Put ('/');
                        end if;
                        Ada.Text_IO.New_Line;
                        Set_Name (Object.BN, No_Name_Component_Sequence);
                     end loop;
                  end if;
               end;

            else
               Ada.Text_IO.Put_Line ("syntax error");
            end if;

         exception
            when E : others =>
               Ada.Text_IO.Put_Line ("exception: "& Exception_Name (E));
         end;
      end if;
   end loop;

end Naming_Sh;
