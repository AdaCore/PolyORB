with Ada.Text_IO;
with Ada.Integer_Text_IO;
package body Controller is

   Max_Workers : Integer := 10;
   type Worker_Table is array (1 .. Max_Workers) of Worker_Access;

   protected Keeper is
      entry Get (W : out Worker_Access);
      entry Put (W : in Worker_Access);
      entry Lock;
      procedure Unlock;
   private
      Count : Integer := 0;
      Table : Worker_Table := (others => null);
      Locked : Boolean := False;
   end Keeper;

   protected body Keeper is
      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;

      procedure Unlock is
      begin
         Locked := False;
      end Unlock;

      entry Get (W : out Worker_Access) when Count > 0 is
      begin
         for Index in Table'Range loop
            if Table (Index) /= null then
               W := Table (Index);
               Table (Index) := null;
               Count := Count - 1;
               exit;
            end if;
         end loop;
      end;

      entry Put (W : in Worker_Access) when Count < Max_Workers is
      begin
         for Index in Table'Range loop
            if Table (Index) = null then
               Count := Count + 1;
               Table (Index) := W;
               exit;
            end if;
         end loop;
      end;
   end Keeper;

   procedure Register (W : Worker_Access) is
   begin
      Keeper.Put (W);
   end Register;

   function Get_Worker return Worker_Access is
      W : Worker_Access;
   begin
      Keeper.Get (W);
      return W;
   end Get_Worker;

   procedure Get_Integer (Message : String; Value : out Integer) is
   begin
      Keeper.Lock;
      Ada.Text_IO.Put (Message);
      Ada.Integer_Text_IO.Get (Value);
      Keeper.Unlock;
   end Get_Integer;

   procedure Done (Message : String) is
   begin
      Ada.Text_IO.Put ("done : ");
      Ada.Text_IO.Put_Line (Message);
   end Done;

end Controller;
