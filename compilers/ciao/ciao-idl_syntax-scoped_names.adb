----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  IDL syntactic information.
--  Helper subprograms for the construction of
--  <scoped_name> nodes.
--  $Id: //depot/ciao/main/ciao-idl_syntax-scoped_names.adb#4 $

with CIAO.Nlists; use CIAO.Nlists;

package body CIAO.IDL_Syntax.Scoped_Names is

   use CIAO.IDL_Tree;

   function Prefixes        (N : in Node_Id)
     return List_Id is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Scoped_Name);
      return List3 (N);
   end Prefixes;

   procedure Add_Prefix
     (N   : Node_Id;
      Val : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Scoped_Name);
      pragma Assert (True
        and then No (Prefix (N))
        and then Node_Kind (Val) = N_Scoped_Name);

      if No (Prefixes (N)) then
         declare
            L : List_Id := New_List;
         begin
            Set_Parent (L, N);
            Set_List3 (N, L);
         end;
      end if;
      Append_To (List3 (N), Val);
   end Add_Prefix;

   procedure Add_Absolute
     (N   : Node_Id) is
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Scoped_Name);
      pragma Assert (True
        and then No (Prefix (N)));

      if No (Prefixes (N)) then
         declare
            L : List_Id := New_List;
         begin
            Set_Parent (L, N);
            Set_List3 (N, L);
         end;
      end if;
      Append_To (List3 (N), New_Node (N_Absolute));
   end Add_Absolute;

   procedure Chain_Prefixes
     (N : in Node_Id) is
      First_Prefix : Node_Id;
   begin
      pragma Assert (False
        or else Node_Kind (N) = N_Scoped_Name);
      pragma Assert (No (Prefix (N)));

      if Is_Empty_List (Prefixes (N)) then
         return;
      end if;

      First_Prefix := Remove_Head (Prefixes (N));
      Set_List3 (First_Prefix, Prefixes (N));
      Set_Node3 (N, Empty);

      Set_Prefix (N, First_Prefix);
      Set_Parent (First_Prefix, N);

      if Node_Kind (First_Prefix) = N_Scoped_Name
        and then not Is_Empty_List (Prefixes (First_Prefix)) then
         Chain_Prefixes (First_Prefix);
      end if;
   end Chain_Prefixes;

end CIAO.IDL_Syntax.Scoped_Names;
