with Ada.Command_Line;     use Ada.Command_Line;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

package body CORBA.Command_Line is

   ----------
   -- Argv --
   ----------

   function Argv return System.Address is
   begin
      return Pd_Argv;
   end Argv;

   ----------------------
   -- Get_Command_Line --
   ----------------------

   function Get_Command_Line
     return System.Address
   is

      type Array_Ptr is access chars_ptr_array;

      Argv_Ptr : Array_Ptr
        := new chars_ptr_array (0 .. size_t (Argument_Count));

      package A2A  is
        new System.Address_To_Access_Conversions (chars_ptr_array);

   begin
      Argv_Ptr.all (0) := New_String ("name_of_the_program");
      for I in 1 .. Argument_Count loop
         declare
            S : size_t := size_t (I);
         begin
            Argv_Ptr.all (S) := New_String (Argument (I));
         end;
      end loop;
      return A2A.To_Address (A2A.Object_Pointer (Argv_Ptr));
   end Get_Command_Line;

begin
   Pd_Argv := Get_Command_Line;
end CORBA.Command_Line;
