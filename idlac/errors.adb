with Ada.Text_IO;

package body Errors is

   ----------------------------
   --  some usefull methods  --
   ----------------------------

   --  counters for errors and warnings
   Error_Count : Natural := 0;
   Warning_Count : Natural := 0;

   --  nice display of a natural
   function Nat_To_String (Val : Natural) return String is
      Res : String := Natural'Image (Val);
   begin
      return Res (2 .. Res'Last);
   end Nat_To_String;

   --  displays a location
   function Display_Location (Loc : in Location) return String is
   begin
--       Ada.Text_IO.Put ("line ");
--       Ada.Text_IO.Put (Nat_To_String (Loc.Line));
--       Ada.Text_IO.Put (", column ");
--       Ada.Text_IO.Put (Nat_To_String (Loc.Col));
--       Ada.Text_IO.Put (" of file ");
--       Ada.Text_IO.Put (Loc.Filename.all);
      return "line " &
        Nat_To_String (Loc.Line) &
        ", column " &
        Nat_To_String (Loc.Col) &
        " of file " &
        Loc.Filename.all;
   end Display_Location;

   --  displays an error
   procedure Display_Error (Message : in String;
                            Level : in Error_Kind;
                            Loc : Location) is
   begin
      case Level is
         when Fatal =>
            Ada.Text_IO.Put ("FATAL ERROR occured");
         when Error =>
            Ada.Text_IO.Put ("ERROR occured");
         when Warning =>
            Ada.Text_IO.Put ("WARNING occured");
      end case;
      if Loc.Line > 0 then
         Ada.Text_IO.Put (" at " & Display_Location (Loc));
      end if;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("    ");
      Ada.Text_IO.Put_Line (Message);
      Ada.Text_IO.New_Line;
   end Display_Error;


   --------------------------------------
   --  functions of the specification  --
   --------------------------------------

   --  deals with a lexer error
   --  displays the error and, depending of its level, raise it
   procedure Lexer_Error (Message : in String;
                          Level : in Error_Kind;
                          Loc : Errors.Location)is
   begin
      case Level is
         when Fatal =>
            null;
         when Error =>
            Error_Count := Error_Count + 1;
         when Warning =>
            Warning_Count := Warning_Count + 1;
      end case;
      Display_Error (Message, Level, Loc);
      if Level = Fatal then
         raise Fatal_Error;
      end if;
   end Lexer_Error;

   --  deals with a parser error
   --  displays the error and, depending of its level, raise it
   procedure Parser_Error (Message : in String;
                           Level : in Error_Kind;
                           Loc : in Location)is
   begin
      case Level is
         when Fatal =>
            null;
         when Error =>
            Error_Count := Error_Count + 1;
         when Warning =>
            Warning_Count := Warning_Count + 1;
      end case;
      Display_Error (Message, Level, Loc);
      if Level = Fatal then
         raise Fatal_Error;
      end if;
   end Parser_Error;

      --  was there any errors ?
   function Is_Error return Boolean is
   begin
      return Error_Count > 0;
   end Is_Error;

   --  was there any warnings ?
   function Is_Warning return Boolean is
   begin
      return Warning_Count > 0;
   end Is_Warning;

   --  returns the number of warnings
   function Warning_Number return Natural is
   begin
      return Error_Count;
   end Warning_Number;

   --  returns the number of errors
   function Error_Number return Natural is
   begin
      return Warning_Count;
   end Error_Number;

end Errors;
