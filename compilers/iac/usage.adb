with Ada.Command_Line; use Ada.Command_Line;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Output; use Output;

procedure Usage is
begin
   Set_Standard_Error;
   Write_Str  ("Usage: ");
   Write_Str  (Command_Name);
   Write_Line (" opts file [-cppargs args]");
   Write_Eol;
   Write_Line ("  name is a file from which you can omit the .idl suffix");
   Write_Eol;
   Write_Line ("  -E       Preprocess only");
   Write_Line ("  -c       Compile only");
   Write_Line ("  -d       Generate delegation package");
   Write_Line ("  -i       Generate implementation template");
   Write_Line ("  -nodyn   Do not generate code for dynamic invocation");
   Write_Line ("  -k       Keep temporary files");
   Write_Line ("  -p       Produce source on standard output");
   Write_Line ("  -q       Be quiet");
   Write_Line ("  -noir    Do not generate code for interface repository");
   Write_Line ("  -cppargs Pass arguments to the C++ preprocessor");
   Write_Line ("  -I dir   Provide a shortcut for -cppargs -I dir");
   Write_Eol;
   OS_Exit (1);
end Usage;
