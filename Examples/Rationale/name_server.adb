with Tapes;
package body Name_Server is

   protected Semaphore is
      entry P;
      procedure V;

   Name_Len : constant := 16;

   type Tape_Info is
      record
         Ptr : Tape_Ptr;
         Name : String (1 .. Name_Len);
      end record;

   Table : array (1 .. 16) of Tape_Info;

   function  Find     (Name : String) return Tape_Ptr is
      Full_Name : String (1 .. Name_Len) := (others => ' ');
   begin
      if Name'Length > Name_Len  then
         raise Constraint_Error;
      else
         Full_Name (1 .. Name'Length) := Name;
      end if;
      for I in Table'Range loop
         if Table (I).Ptr /= null and then
            Table (I).Name = Full_Name then
            return Table (I).Ptr;
         end if;
      end loop;
      return null;
   end Find;
		
   procedure Register (Name : in String; T : in Tape_Ptr) is
      Full_Name : String (1 .. Name_Len) := (others => ' ');
   begin
      if Name'Length > Name_Len  then
         raise Constraint_Error;
      else
         Full_Name (1 .. Name'Length) := Name;
      end if;
      for I in Table'Range loop
         if Table (I).Ptr = null then
            Table (I).Name := Full_Name;
            Table (I).Ptr  := T;
            return;
         end if;
      end loop;
      raise Constraint_Error;
   end Register;
   
   procedure Remove   (T : in Tape_Ptr) is
   begin
      for I in Table'Range loop
         if Table (I).Ptr = T then
            Table (I).Ptr := null;
            return;
         end if;
      end loop;
      raise Constraint_Error;
   end Remove;

end Name_Server;
