------------------------------------------------------------------------------
--                                                                          --
--                                                                          --
--                          DROOPI  COMPONENTS                              --
--
--                           Exception Stack body                           --
--                               --
--                                                                          -
------------------------------------------------------------------------------



with Droopi.Log;
with Droopi.Soft_Links; use Droopi.Soft_Links;
with Ada.Unchecked_Deallocation;

package body CORBA.Exceptions.Stack is


   package L is new Droopi.Log.Facility_Log ("corba.exceptions.stack");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Magic : constant String := "AB_Exc_Occ";

   type IDL_Exception_Members_Ptr is
      access all IDL_Exception_Members'Class;

   type Exc_Occ_Id_Type is new Natural;

   Seed_Id : Exc_Occ_Id_Type := 1;
   Null_Id : constant Exc_Occ_Id_Type := 0;

   type Exc_Occ_Node;
   type Exc_Occ_List is access Exc_Occ_Node;
   type Exc_Occ_Node is
      record
         Id   : Exc_Occ_Id_Type;
         Mbr  : IDL_Exception_Members_Ptr;
         Next : Exc_Occ_List;
      end record;

   Exc_Occ_Head : Exc_Occ_List;
   Exc_Occ_Tail : Exc_Occ_List;

   Exc_Occ_List_Size     : Natural := 0;
   Max_Exc_Occ_List_Size : constant Natural := 1000;

   procedure Free is
      new Ada.Unchecked_Deallocation
        (Exc_Occ_Node, Exc_Occ_List);

   procedure Free is
      new Ada.Unchecked_Deallocation
        (IDL_Exception_Members'Class,
         IDL_Exception_Members_Ptr);

   function Image (V : Exc_Occ_Id_Type) return String;
   function Value (M : String) return Exc_Occ_Id_Type;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (Exc_Occ : in CORBA.Exception_Occurrence;
      Exc_Mbr : out IDL_Exception_Members'Class)
   is
      Exc_Occ_Id : Exc_Occ_Id_Type;
      Current    : Exc_Occ_List;
      Previous   : Exc_Occ_List;

   begin
      Enter_Critical_Section;
      Exc_Occ_Id := Value (Ada.Exceptions.Exception_Message (Exc_Occ));
      if Exc_Occ_Id = Null_Id then
         Leave_Critical_Section;
         return;
      end if;

      Current := Exc_Occ_Head;
      while Current /= null loop
         exit when Current.Id = Exc_Occ_Id;

         Previous := Current;
         Current  := Current.Next;
      end loop;

      if Current = null then
         Leave_Critical_Section;
         Broca.Exceptions.Raise_Imp_Limit;
      end if;

      if Previous /= null then
         Previous.Next := Current.Next;

      else
         Exc_Occ_Head := Current.Next;
      end if;

      Exc_Mbr := Current.Mbr.all;

      Free (Current.Mbr);
      Free (Current);
      Exc_Occ_List_Size := Exc_Occ_List_Size - 1;
      Leave_Critical_Section;

   exception
      when others =>
         if Current /= null then
            if Current.Mbr /= null then
               Free (Current.Mbr);
            end if;
            Free (Current);
         end if;
         Exc_Occ_List_Size := Exc_Occ_List_Size - 1;
         Leave_Critical_Section;
         raise;
   end Get_Members;

   -----------
   -- Image --
   -----------

   function Image (V : Exc_Occ_Id_Type) return String is
   begin
      return Magic & V'Img;
   end Image;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (Exc_Id  : in Ada.Exceptions.Exception_Id;
      Exc_Mbr : in IDL_Exception_Members'Class)
   is
      Current    : Exc_Occ_List;
      Exc_Occ_Id : Exc_Occ_Id_Type;

   begin
      Enter_Critical_Section;
      if Exc_Occ_List_Size = Max_Exc_Occ_List_Size then
         Current := Exc_Occ_Head;
         Exc_Occ_Head := Exc_Occ_Head.Next;
         if Current.Mbr /= null then
            Free (Current.Mbr);
         end if;

      else
         Exc_Occ_List_Size := Exc_Occ_List_Size + 1;
         Current := new Exc_Occ_Node;
      end if;

      Exc_Occ_Id   := Seed_Id;
      Current.Id   := Seed_Id;
      Current.Mbr  := new IDL_Exception_Members'Class'(Exc_Mbr);
      Current.Next := null;

      if Seed_Id = Exc_Occ_Id_Type'Last then
         Seed_Id := Null_Id;
      end if;
      Seed_Id := Seed_Id + 1;

      if Exc_Occ_Head = null then
         Exc_Occ_Head := Current;
      end if;
      Current.Next := Exc_Occ_Tail;
      Exc_Occ_Tail := Current;
      Leave_Critical_Section;

      Ada.Exceptions.Raise_Exception (Exc_Id, Image (Exc_Occ_Id));
      raise Program_Error;
   end Raise_Exception;

   -----------
   -- Value --
   -----------

   function Value (M : String) return Exc_Occ_Id_Type is
      V : Exc_Occ_Id_Type := 0;
      N : Natural := M'First;

   begin
      if M'Length <= Magic'Length + 1 then
         return Null_Id;
      end if;

      for I in Magic'Range loop
         if Magic (I) /= M (N) then
            return Null_Id;
         end if;
         N := N + 1;
      end loop;

      if M (N) /= ' ' then
         return Null_Id;
      end if;
      N := N + 1;

      while N <= M'Last loop
         if M (N) not in '0' .. '9' then
            return Null_Id;
         end if;

         V := V * 10 + Character'Pos (M (N)) - Character'Pos ('0');
         N := N + 1;
      end loop;

      return V;
   end Value;

end  CORBA.Exceptions.Stack;
