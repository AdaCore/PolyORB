with Tapes;
package body Name_Server is

   protected Semaphore is
      entry P;
      procedure V;
   private
      Locked : Boolean := False;
   end Semaphore;

   protected body Semaphore is
      entry P when not Locked is
      begin
         Locked := True;
      end P;
      procedure V is 
      begin
         Locked := False;
      end V;
   end Semaphore;

   Name_Len : constant := 16;
   subtype Name_Type is String (1 .. Name_Len);

   type Tape_Info is
      record
         Ptr  : Tape_Ptr;
         Name : Name_Type;
      end record;

   Table : array (1 .. 16) of Tape_Info;

   function  Name (T : in Tape_Ptr) return String is
      N : Name_Type;
   begin
      Semaphore.P;
      for I in Table'Range loop
         if Table (I).Ptr = T then
            N := Table (I).Name;
            Semaphore.V;
            for L in N'Range loop
               if N (L) = Ascii.Nul then
                  return N (1 .. L - 1);
               end if;
            end loop;
         end if;
      end loop;
      Semaphore.V;
      raise Constraint_Error;
   end Name;
  
   function  Find (Name : String) return Tape_Ptr is
      N : Name_Type := (others => Ascii.Nul);
      P : Tape_Ptr;
   begin
      if Name'Length > Name_Len  then
         raise Constraint_Error;
      else
         N (1 .. Name'Length) := Name;
      end if;
      Semaphore.P;
      for I in Table'Range loop
         if Table (I).Ptr /= null and then
            Table (I).Name = N then
            P := Table (I).Ptr;
            Semaphore.V;
            return P;
         end if;
      end loop;
      Semaphore.V;
      return null;
   end Find;
		
   procedure Register (Name : in String; T : in Tape_Ptr) is
      N : Name_Type := (others => Ascii.Nul);
   begin
      if Name'Length > Name_Len  then
         raise Constraint_Error;
      else
         N (1 .. Name'Length) := Name;
      end if;
      Semaphore.P;
      for I in Table'Range loop
         if Table (I).Ptr = null then
            Table (I).Name := N;
            Table (I).Ptr  := T;
            Semaphore.V;
            return;
         end if;
      end loop;
      Semaphore.V;
      raise Constraint_Error;
   end Register;
   
   procedure Remove   (T : in Tape_Ptr) is
   begin
      Semaphore.P;
      for I in Table'Range loop
         if Table (I).Ptr = T then
            Table (I).Ptr := null;
            Semaphore.V;
            return;
         end if;
      end loop;
      Semaphore.V;
      raise Constraint_Error;
   end Remove;

end Name_Server;
