------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                I D L _ F E . T R E E . S Y N T H E T I C                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;

with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Display_Tree;
with Idl_Fe.Debug;
pragma Elaborate_All (Idl_Fe.Debug);

with Idlac_Errors; use Idlac_Errors;

package body Idl_Fe.Tree.Synthetic is

   -----------
   -- Debug --
   -----------

   Flag : constant Natural := Idl_Fe.Debug.Is_Active ("idl_fe.tree.synthetic");
   procedure O is new Idl_Fe.Debug.Output (Flag);

   function Is_Interface_Type
     (Node : Node_Id; Or_ValueType : Boolean := False) return Boolean is
   begin
      pragma Debug (O ("Is_Interface_Type: enter, dealing with a "
                    & Node_Kind'Image (Kind (Node))));
      case Kind (Node) is
         when
           K_Interface         |
           K_Forward_Interface =>
            return True;

         when
           K_ValueType         |
           K_Forward_ValueType =>
            return Or_ValueType;

         when K_Scoped_Name =>
            return Is_Interface_Type (S_Type (Node));

         when K_Declarator =>
            declare
               P_Node : constant Node_Id := Parent (Node);
            begin
               pragma Assert (Is_Type_Declarator (P_Node));

               return Is_Empty (Array_Bounds (Node))
                 and then Is_Interface_Type (T_Type (P_Node));
            end;

         when others =>
            return False;
      end case;

   end Is_Interface_Type;

   function Is_Gen_Scope
     (Node : Node_Id)
     return Boolean
   is
      K : constant Node_Kind
        := Kind (Node);
   begin
      return (False
        or else K = K_Repository
        or else K = K_Ben_Idl_File
        or else K = K_Module
        or else K = K_Interface
        or else K = K_ValueType);
   end Is_Gen_Scope;

   function Name
     (Node : Node_Id)
     return String is
   begin
      if Definition (Node) /= null then
         return Definition (Node).Name.all;
      elsif True
        and then (Kind (Node) = K_Forward_Interface
                  or else Kind (Node) = K_Forward_ValueType)
        and then Forward (Node) /= No_Node
      then
         return Name (Forward (Node));
      else
         return "##null##";
      end if;
   end Name;

   function Original_Operation_Type
     (Node : Node_Id)
     return Node_Id
   is
      OT_Node : constant Node_Id
        := Operation_Type (Node);
      Original_OT_Node : constant Node_Id
        := Original_Node (OT_Node);
   begin
      if Original_OT_Node /= No_Node then
         return Original_OT_Node;
      else
         return OT_Node;
      end if;
   end Original_Operation_Type;

   function Parent_Scope
     (Node : Node_Id)
     return Node_Id
   is
      Override : constant Node_Id
        := Parent_Scope_Override (Node);
   begin
      if Override /= No_Node then
         return Override;
      else
         return Original_Parent_Scope (Node);
      end if;
   end Parent_Scope;

   function Original_Parent_Scope
     (Node : Node_Id)
     return Node_Id is
   begin
      if Definition (Node) /= null then
         return Definition (Node).Parent_Scope;

      elsif (Kind (Node) = K_Forward_Interface
             or else Kind (Node) = K_Forward_ValueType)
        and then Forward (Node) /= No_Node
      then
         return Original_Parent_Scope (Forward (Node));

      else
         return No_Node;
      end if;
   end Original_Parent_Scope;

   procedure Set_Parent_Scope
     (Node : Node_Id;
      To : Node_Id) is
   begin
      Set_Parent_Scope_Override (Node, To);
   end Set_Parent_Scope;

   function Idl_Repository_Id
     (Node : Node_Id)
     return String
   is
      Repository_Id_Node : constant Node_Id
        := Repository_Id (Node);
   begin
      --  XXX pragma Assert (Repository_Id_Node /= No_Node);
      --  Not verified with current version of CIAO.

      if Repository_Id_Node = No_Node then
         return "<unknown repository id>";
      end if;
      return String_Value (Repository_Id_Node);

   end Idl_Repository_Id;

   function Version (Node : Node_Id) return String is
      Id : constant String := Idl_Repository_Id (Node);
      Colon : Integer := Id'Last;
   begin
      while Colon >= Id'First and then Id (Colon) /= ':' loop
         Colon := Colon - 1;
      end loop;

      if Colon < Id'First then
         Error ("Cannot determine version, rid=«" & Id & "»",
                Fatal, Get_Location (Node));
      end if;

      return Id (Colon + 1 .. Id'Last);
   end Version;

   function All_Ancestors
     (Node : Node_Id;
      Exclude : Node_List := Nil_List)
     return Node_List
   is
      It : Node_Iterator;
      I_Node : Node_Id;
      --  A scoped name in the inheritance spec.

      P_Node : Node_Id;
      --  The corresponding actual parent node.

      Result : Node_List
        := Nil_List;
   begin
      Init (It, Parents (Node));

      while not Is_End (It) loop
         Get_Next_Node (It, I_Node);

         P_Node := Value (I_Node);
         if not (False
           or else Is_In_List (Exclude, P_Node)
           or else Is_In_List (Result, P_Node)) then
            Append_Node (Result, P_Node);
            Merge_List
              (Into => Result,
               From => All_Ancestors (P_Node));
         end if;
      end loop;

      return Result;
   end All_Ancestors;

   ---------------------
   --  Primary_Parent --
   ---------------------
   function Primary_Parent (Node : Node_Id) return Node_Id is
      It : Node_Iterator;
      Candidate : Node_Id;
   begin
      pragma Assert ((Kind (Node) = K_Interface)
                     or else (Kind (Node) = K_ValueType));
      Init (It, Parents (Node));
      while not Is_End (It) loop
         Get_Next_Node (It, Candidate);
         if not Abst (Value (Candidate)) then
            return Candidate;
         end if;
      end loop;
      return No_Node;
   end Primary_Parent;

   ---------------------------------------
   --  Supports_Non_Abstract_Interface  --
   ---------------------------------------
   function Supports_Non_Abstract_Interface (Node : Node_Id)
     return Boolean is
      It : Node_Iterator;
      Current : Node_Id;
   begin
      pragma Assert (Kind (Node) = K_ValueType);
      Init (It, Supports (Node));
      while not Is_End (It) loop
         Get_Next_Node (It, Current);
         --  we get a K_Scoped_Name that we must transform into K_Interface
         if not Abst (Value (Current)) then
            return True;
         end if;
      end loop;
      return False;
   end Supports_Non_Abstract_Interface;

   -----------------------------
   -- Has_Interface_Component --
   -----------------------------

   function Has_Interface_Component
     (Node   : Node_Id;
      I_Node : Node_Id) return Boolean is
   begin
      pragma Assert (Kind (I_Node) = K_Interface);
      case Kind (Node) is
         when K_Void
            | K_Float
            | K_Double
            | K_Long_Double
            | K_Short
            | K_Long
            | K_Long_Long
            | K_Unsigned_Short
            | K_Unsigned_Long
            | K_Unsigned_Long_Long
            | K_Char
            | K_Wide_Char
            | K_Boolean
            | K_Octet
            | K_Any
            | K_Object
            | K_Enum
            | K_ValueType
            | K_Forward_ValueType
            | K_Boxed_ValueType
            | K_String
            | K_Wide_String
            | K_Fixed
            | K_Forward_Interface
            | K_Sequence_Instance
            | K_Sequence =>

            return False;

         when K_Interface =>
            return Node = I_Node;

         when K_Struct
            | K_Exception =>
            declare
               Iter   : Node_Iterator;
               Member : Node_Id;

            begin
               Init (Iter, Members (Node));

               while not Is_End (Iter) loop
                  Get_Next_Node (Iter, Member);

                  if Has_Interface_Component (M_Type (Member), I_Node) then
                     return True;
                  end if;
               end loop;

               return False;
            end;

         when K_Union =>
            declare
               Iter      : Node_Iterator;
               Case_Node : Node_Id;

            begin
               Init (Iter, Cases (Node));

               while not Is_End (Iter) loop
                  Get_Next_Node (Iter, Case_Node);

                  if Has_Interface_Component
                       (Case_Type (Case_Node), I_Node)
                  then
                     return True;
                  end if;
               end loop;

               return False;
            end;

         when K_Scoped_Name =>
            return Has_Interface_Component (Value (Node), I_Node);

         when K_Declarator =>
            return Has_Interface_Component (Parent (Node), I_Node);

         when K_Type_Declarator =>
            return Has_Interface_Component (T_Type (Node), I_Node);

         when others =>
            Ada.Text_IO.Put_Line (Node_Kind'Image (Kind (Node)));
            Display_Tree.Disp_Tree (Node);
            raise Program_Error;
            return False;
      end case;
   end Has_Interface_Component;

   -------------------------
   -- Has_Local_Component --
   -------------------------

   function Has_Local_Component (Node : Node_Id) return Boolean is
   begin
      case Kind (Node) is
         when K_Void
            | K_Float
            | K_Double
            | K_Long_Double
            | K_Short
            | K_Long
            | K_Long_Long
            | K_Unsigned_Short
            | K_Unsigned_Long
            | K_Unsigned_Long_Long
            | K_Char
            | K_Wide_Char
            | K_Boolean
            | K_Octet
            | K_Any
            | K_Object
            | K_Enum
            | K_ValueType
            | K_Forward_ValueType
            | K_Boxed_ValueType
            | K_String
            | K_String_Instance
            | K_Wide_String
            | K_Fixed =>
            return False;

         when K_Interface
            | K_Forward_Interface =>
            return Local (Node);

         when K_Struct
            | K_Exception =>
            declare
               Iter   : Node_Iterator;
               Member : Node_Id;

            begin
               Init (Iter, Members (Node));

               while not Is_End (Iter) loop
                  Get_Next_Node (Iter, Member);

                  if Has_Local_Component (M_Type (Member)) then
                     return True;
                  end if;
               end loop;

               return False;
            end;

         when K_Union =>
            declare
               Iter      : Node_Iterator;
               Case_Node : Node_Id;

            begin
               Init (Iter, Cases (Node));

               while not Is_End (Iter) loop
                  Get_Next_Node (Iter, Case_Node);

                  if Has_Local_Component (Case_Type (Case_Node)) then
                     return True;
                  end if;
               end loop;

               return False;
            end;

         when K_Scoped_Name =>
            return Has_Local_Component (Value (Node));

         when K_Declarator =>
            return Has_Local_Component (Parent (Node));

         when K_Type_Declarator =>
            return Has_Local_Component (T_Type (Node));

         when K_Sequence_Instance =>
            return Has_Local_Component (Sequence (Node));

         when K_Sequence =>
            return Has_Local_Component (Sequence_Type (Node));

         when others =>
            Ada.Text_IO.Put_Line (Node_Kind'Image (Kind (Node)));
            Display_Tree.Disp_Tree (Node);
            raise Program_Error;
            return False;
      end case;
   end Has_Local_Component;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value (Node : Node_Id) return Idl_Integer is
   begin
      return Expr_Value (Node).Integer_Value;
   end Integer_Value;

   function Character_Value
     (Node : Node_Id)
     return Character is
   begin
      return Character (Expr_Value (Node).Char_Value);
   end Character_Value;

   function Float_Value
     (Node : Node_Id)
     return Idl_Float is
   begin
      return Idl_Float (Expr_Value (Node).Float_Value);
   end Float_Value;

   function String_Value
     (Node : Node_Id)
     return String is
   begin
      return Expr_Value (Node).String_Value.all;
   end String_Value;

   function WString_Value
     (Node : Node_Id)
     return Wide_String is
   begin
      return Expr_Value (Node).WString_Value.all;
   end WString_Value;

   function Boolean_Value
     (Node : Node_Id)
     return Boolean is
   begin
      return Expr_Value (Node).Boolean_Value;
   end Boolean_Value;

   function Enum_Value
     (Node : Node_Id)
     return Node_Id is
   begin
      return Expr_Value (Node).Enum_Value;
   end Enum_Value;

   procedure Set_String_Value
     (Node : Node_Id;
      Val  : String) is
   begin
      Set_Expr_Value
        (Node, new Constant_Value (Kind => C_String));
      Expr_Value (Node).String_Value := new String'(Val);
   end Set_String_Value;

   function Default_Repository_Id
     (Node : Node_Id)
     return String
   is
      P_Node : constant Node_Id
        := Parent_Scope (Node);
   begin
      if P_Node /= No_Node then
         if Kind (P_Node) = K_Repository then
            return Name (Node);
         else
            return Default_Repository_Id (Parent_Scope (Node))
              & "/" & Name (Node);
         end if;
      else
         return Name (Node);
      end if;
   end Default_Repository_Id;

   function S_Type (Node : Node_Id) return Node_Id is
      Typ : Node_Id := Value (Node);
   begin
      if Kind (Typ) = K_Declarator then

         --  For a typedef, go back to the original type

         if Kind (Parent (Typ)) = K_Type_Declarator then
            pragma Debug (O ("S_Type: the name is defined in a typedef"));

            if not Is_Empty (Array_Bounds (Typ)) then
               return Typ;
            end if;

            Typ := T_Type (Parent (Typ));
            if Kind (Typ) = K_Scoped_Name then
               return S_Type (Typ);
            else
               return Typ;
            end if;

         elsif Kind (Parent (Typ)) = K_Native then
            return Parent (Typ);
         end if;

      elsif Kind (Typ) = K_Struct
        or else Kind (Typ) = K_Union
        or else Kind (Typ) = K_Enum
        or else Kind (Typ) = K_Interface
        or else Kind (Typ) = K_ValueType
        or else Kind (Typ) = K_Forward_Interface
        or else Kind (Typ) = K_Boxed_ValueType
        or else Kind (Typ) = K_Forward_ValueType
        or else Kind (Typ) = K_Sequence_Instance
        or else Kind (Typ) = K_String_Instance
      then
         return Typ;
      end if;

      Error ("Scoped name does not denote a type",
             Fatal, Get_Location (Node));

      --  Not reached

      return No_Node;
   end S_Type;

end Idl_Fe.Tree.Synthetic;
