with Broca.Soft_Links;

package body Broca.Value.Operation_Store is

   type Operation_Store;
   type Operation_Store_Ptr is access all Operation_Store'Class;

   type Operation_Store is tagged record
      Tag : Ada.Tags.Tag;
      Operation : Operation_Type;
      Next : Operation_Store_Ptr;
   end record;

   The_Store : Operation_Store_Ptr := null;

   -------------------------
   --  Register_Operation --
   -------------------------
   procedure Register_Operation (T : in Ada.Tags.Tag;
                                 Op : in Operation_Type) is
   begin
      Broca.Soft_Links.Enter_Critical_Section;
      The_Store := new Operation_Store'
        ( Tag => T,
          Operation => Op,
          Next => The_Store);
      Broca.Soft_Links.Leave_Critical_Section;
   end Register_Operation;

   --------------------
   --  Get_Operation --
   --------------------
   function Get_Operation (T : in Ada.Tags.Tag)
                           return Operation_Type is
      Temp : Operation_Store_Ptr;
   begin
      Temp := The_Store;
      Broca.Soft_Links.Enter_Critical_Section;
      while Temp /= null loop
         exit when Ada.Tags."=" (Temp.Tag, T);
         Temp := Temp.Next;
      end loop;
      if Temp = null then
         Broca.Exceptions.Raise_Internal;
      end if;
      Broca.Soft_Links.Leave_Critical_Section;
      return Temp.Operation;
   end Get_Operation;


end Broca.Value.Operation_Store;
