with Ada.Text_IO; use Ada.Text_IO;

package body AdaBroker.Debug is

   Filename : constant String := "adabroker.deb";

   type String_Ptr is access String;

   Flag_Table : array (1 .. 32) of String_Ptr;
   Last_Flag  : Natural := 0;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active
     (Flag : in String)
      return Natural
   is
   begin
      for I in 1 .. Last_Flag loop
         if Flag_Table (I).all = Flag then
            return I;
         end if;
      end loop;
      return 0;
   end Is_Active;

   ------------
   -- Output --
   ------------

   procedure Output
     (Message : in String)
   is
   begin
      if Flag /= 0 then
         Put_Line (Flag_Table (Flag).all & " : " & Message);
      end if;
   end Output;

   File : File_Type;
   Line : String (1 .. 256);
   Last : Natural;

begin
   begin
      Open (File, In_File, Filename);

      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);
         if Last /= 0 then
            if Line (1) /= '#' then
               if Is_Active (Line (1 .. Last)) = 0 then
                  Last_Flag := Last_Flag + 1;
                  Flag_Table (Last_Flag) := new String'(Line (1 .. Last));
               end if;
            end if;
         end if;
      end loop;

      Close (File);

   exception
      when others =>
         pragma Debug
           (Put_Line ("** No debugging options file : " & Filename & " **"));
      null;
   end;
end AdaBroker.Debug;




