with Common; use Common;

package body Controller is

   --  We insert each distributed object in a group. When
   --  Max distributed objects have been inserted, we can
   --  provide to each distributed object its neighbor.
   --  Otherwise, function Next is a blocking operation.
   --  This is done using a protected object.

   type Pool_Info is record
      Pool : Pool_Access;
      PID  : Partition_ID;
   end record;

   Max : constant := 3;
   type Pool_Info_Table is array (1 .. Max) of Pool_Info;

   protected Group is
      procedure Add  (Pool : in  Pool_Access; PID : Partition_ID);
      entry     Next (Pool : out Pool_Access; PID : Partition_ID);
   private
      Table : Pool_Info_Table;
      Last  : Natural := 0;
   end Group;

   protected body Group is
      procedure Add  (Pool : in  Pool_Access; PID : Partition_ID) is
      begin
         Last := Last + 1;
         Table (Last) := (Pool, PID);
      end Add;
      entry     Next (Pool : out Pool_Access; PID : Partition_ID)
      when Last = Max is
      begin
         for Index in 1 .. Max loop
            if Table (Index).PID = PID then
               if Index = Max then
                  Pool := Table (1).Pool;
               else
                  Pool := Table (Index + 1).Pool;
               end if;
               exit;
            end if;
         end loop;
      end Next;
   end Group;

   function Register
     (Pool : Pool_Access; PID : Partition_ID)
      return Boolean is
   begin
      Group.Add (Pool, PID);
      return True;
   end Register;

   function  Next
     (PID : Partition_ID)
      return Pool_Access is
      Pool : Pool_Access;
   begin
      Group.Next (Pool, PID);
      return Pool;
   end Next;

end Controller;
