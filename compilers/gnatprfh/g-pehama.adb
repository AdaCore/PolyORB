with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

with GNAT.Perfect_Hash.Generators; use GNAT.Perfect_Hash.Generators;

procedure GNAT.Perfect_Hash.Main is

   type String_Access is access String;

   File      : File_Type;
   Buffer    : String (1 .. 256);
   Last      : Natural;

   Filename : String_Access;
   K_To_V   : Float;
   Pkg_Name : String_Access;
   Position : String_Access;
   Optim    : Optimization := Memory_Space;

   procedure Parse_Command_Line;

   Flag : Character := '-';

   procedure Check (B : Boolean);

   -----------
   -- Check --
   -----------

   procedure Check (B : Boolean) is
   begin
      if not B then
         Put_Line (Standard_Error, "cannot parse command line");
         raise Program_Error;
      end if;
   end Check;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      Argc : constant Natural := Argument_Count;

   begin
      for I in 1 .. Argc loop
         declare
            Arg : constant String := Argument (I);
            Len : constant Natural := Arg'Length;

         begin
            if Arg (1) = Flag then
               if Len = 1 then
                  Check ((Filename = null));
                  Filename := new String'("");

               else
                  case Arg (2) is
                     when '-' =>
                        Check ((Len = 2));
                        Flag := ASCII.NUL;

                     when 'v' =>
                        Check ((Len = 2));
                        Verbose := True;

                     when 's' =>
                        Check ((Position = null));
                        Position := new String'(Arg (3 .. Len));

                     when 'p' =>
                        Check ((Pkg_Name = null));
                        Pkg_Name := new String'(Arg (3 .. Len));

                     when 'm' =>
                        Optim := Memory_Space;

                     when 'c' =>
                        Optim := CPU_Time;

                     when others =>
                        null;
                  end case;
               end if;

            else
               Check ((Filename = null));
               Filename := new String'(Arg);
            end if;
         end;
      end loop;

      if Pkg_Name = null then
         Pkg_Name := new String'(Default_Pkg_Name);
      end if;

      if Position = null then
         Position := new String'(Default_Position);
      end if;

      K_To_V := Default_K_To_V;
   end Parse_Command_Line;

begin
   Parse_Command_Line;

   if Filename = null then
      Put_Line (Standard_Error, "Usage: grmphf opts name");
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "   name is a filename of words");
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "grmphf switches:");
      Put_Line (Standard_Error, "  -        Standard input");
      Put_Line (Standard_Error, "  -v       Verbose mode");
      Put_Line (Standard_Error, "  -sRANGE  Char selection");
      Put_Line (Standard_Error, "  -pNAME   Package name");
      Put_Line (Standard_Error, "  -c       CPU time optimization");
      Put_Line (Standard_Error, "  -m       Memory space optimization");
      return;
   end if;

   Initialize (4321, K_To_V, Optim);

   if Filename'Length /= 0 then
      Open (File, In_File, Filename.all);
      Set_Input (File);
   end if;

   while not End_Of_File (Current_Input) loop
      Get_Line (Buffer, Last);
      Insert (Buffer (1 .. Last));
   end loop;

   Compute (Position.all);
   Produce (Pkg_Name.all);

end GNAT.Perfect_Hash.Main;
