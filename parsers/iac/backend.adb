with GNAT.OS_Lib;  use GNAT.OS_Lib;

with Errors;  use Errors;
with Output;  use Output;

package body Backend is

   type Backend_Record is record
      Language  : String_Access;
      Comments  : String_Access;
      Generate  : Generate_Procedure;
      Configure : Configure_Procedure;
   end record;

   Table   : array (1 .. 8) of Backend_Record;
   First   : constant Natural := Table'First;
   Last    : Natural := 0;
   Current : Natural := 0;

   ---------------
   -- Configure --
   ---------------

   procedure Configure is
   begin
      if Current /= 0 then
         Table (Current).Configure.all;
      end if;
   end Configure;

   ----------------------
   -- Current_Language --
   ----------------------

   function Current_Language return String is
   begin
      return Table (Current).Language.all;
   end Current_Language;

   --------------
   -- Generate --
   --------------

   procedure Generate (Root : Node_Id) is
   begin
      if Current /= 0 then
         Table (Current).Generate (Root);
      end if;
   end Generate;

   -----------------------
   -- Is_Valid_Language --
   -----------------------

   function Is_Valid_Language (L : String) return Boolean is
   begin
      for N in First .. Last loop
         if Table (N).Language.all = L then
            return True;
         end if;
      end loop;
      return False;
   end Is_Valid_Language;

   --------------
   -- Register --
   --------------

   procedure Register
     (Generate  : Generate_Procedure;
      Configure : Configure_Procedure;
      Language  : String;
      Comments  : String)
   is
   begin
      if Last >= Table'Last then
         DE ("too many target languages");
      end if;
      for I in First .. Last loop
         if Table (I).Language.all = Language then
            DE ("already declared target language");
            raise Fatal_Error;
         end if;
      end loop;
      Last := Last + 1;
      Table (Last).Generate  := Generate;
      Table (Last).Configure := Configure;
      Table (Last).Language  := new String'(Language);
      Table (Last).Comments  := new String'(Comments);
   end Register;

   --------------------------
   -- Set_Current_Language --
   --------------------------

   procedure Set_Current_Language (Language : String) is
   begin
      Current := 0;
      for I in First .. Last loop
         if Table (I).Language.all = Language then
            Current := I;
            exit;
         end if;
      end loop;
      if Current = 0 then
         DE ("unknown target language");
         raise Fatal_Error;
      end if;
   end Set_Current_Language;

   ---------------------
   -- Write_Languages --
   ---------------------

   procedure Write_Languages
     (L, C : Natural)
   is
      S : String (1 .. 64);
   begin
      for I in reverse First .. Last loop
         S := (others => ' ');
         declare
            Language : constant String := Table (I).Language.all;
            Comments : constant String := Table (I).Comments.all;
         begin
            S (L .. L + Language'Length - 1) := Language;
            S (C .. C + Comments'Length - 1) := Comments;
            Write_Line (S (1 .. C + Comments'Length - 1));
         end;
      end loop;
   end Write_Languages;

end Backend;

