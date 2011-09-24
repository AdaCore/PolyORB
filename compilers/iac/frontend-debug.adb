------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       F R O N T E N D . D E B U G                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2011, Free Software Foundation, Inc.          --
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

with Charset;     use Charset;
with Locations;   use Locations;
with Namet;       use Namet;
with Scopes;      use Scopes;
with Utils;       use Utils;
with Values;      use Values;

with Frontend.Nutils; use Frontend.Nutils;

package body Frontend.Debug is

   -----------
   -- Image --
   -----------

   function Image (N : Node_Kind) return String is
      S : String := Node_Kind'Image (N);
   begin
      To_Lower (S);
      for I in S'Range loop
         if S (I) = '_' then
            S (I) := ' ';
         end if;
      end loop;
      return S (3 .. S'Last);
   end Image;

   function Image (N : Name_Id) return String is
   begin
      if N = No_Name then
         return "";
      else
         return Get_Name_String (N);
      end if;
   end Image;

   function Image (N : Node_Id) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : List_Id) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : Mode_Id) return String is
   begin
      return Quoted (Image (Parameter_Mode (N)));
   end Image;

   function Image (N : Pragma_Type) return String is
   begin
      return Quoted (Image (Get_Pragma_Type (N)));
   end Image;

   function Image (N : Value_Id) return String is
   begin
      return Values.Image (N);
   end Image;

   function Image (N : Operator_Id) return String is
   begin
      return Quoted (Image (Token_Type'Val (N)));
   end Image;

   function Image (N : Boolean) return String is
   begin
      return Boolean'Image (N);
   end Image;

   function Image (N : Byte) return String is
   begin
      return Image (Int (N));
   end Image;

   ---------------
   -- W_Boolean --
   ---------------

   procedure W_Boolean (N : Boolean) is
   begin
      Write_Str (N'Img);
   end W_Boolean;

   ------------
   -- W_Byte --
   ------------

   procedure W_Byte (N : Byte) is
   begin
      Write_Int (Int (N));
   end W_Byte;

   -----------------
   -- W_Full_Tree --
   -----------------

   procedure W_Full_Tree is
      D : Node_Id := First_Entity (Definitions (IDL_Spec));
   begin
      N_Indents := 0;
      while Present (D) loop
         W_Node_Id (D);
         D := Next_Entity (D);
      end loop;
   end W_Full_Tree;

   ---------------
   -- W_Indents --
   ---------------

   procedure W_Indents is
   begin
      for I in 1 .. N_Indents loop
         Write_Str ("   ");
      end loop;
   end W_Indents;

   ---------------
   -- W_List_Id --
   ---------------

   procedure W_List_Id (L : List_Id) is
      E : Node_Id;
   begin
      if L = No_List then
         return;
      end if;

      E := First_Entity (L);
      while E /= No_Node loop
         W_Node_Id (E);
         E := Next_Entity (E);
      end loop;
   end W_List_Id;

   ----------------------
   -- W_Node_Attribute --
   ----------------------

   procedure W_Node_Attribute
     (A : String;
      K : String;
      V : String;
      N : Int := 0)
   is
      C : Node_Id;
   begin
      if A = "Next_Entity"
        or else A = "Homonym"
        or else A = "Name"
        or else A = "Visible"
        or else A = "Implicitly_Visible"
        or else A = "Scoped_Identifiers"
        or else A = "Next_Identifier"
      then
         return;
      end if;
      N_Indents := N_Indents + 1;
      W_Indents;
      Write_Str  (A);
      Write_Char (' ');
      Write_Str  (K);
      Write_Char (' ');
      C := Node_Id (N);
      if K = "Name_Id" then
         Write_Line (Quoted (V));

      --  If the attribute name is BE_Node, we don't want to call Kind (the
      --  front-end one) on it, because it's (conceptually) the wrong type!

      elsif K = "Node_Id"
        and then Present (C)
        and then A /= "BE_Node"
      then
         if C > Frontend.Nodes.Entries.Last then
            Write_Str ("*** invalid Node_Id: ");
            Write_Line (V);
         else
            case Kind (C) is
               when K_Float .. K_Value_Base =>
                  Write_Line ('(' & Image (Kind (Node_Id (N))) & ')');
               when others =>
                  Write_Line (V);
            end case;
         end if;

      else
         Write_Line (V);
      end if;

      if A /= "Corresponding_Entity"
        and then A /= "Scope_Entity"
        and then A /= "Potential_Scope"
        and then A /= "Reference"
        and then A /= "Base_Interface"
        and then A /= "Declaration"
        and then A /= "BE_Node"
        and then A /= "Type_Id"
        and then A /= "Type_Prefix"
        and then A /= "Type_Version"
      then
         if K = "Node_Id" then
            W_Node_Id (Node_Id (N));
         elsif K = "List_Id" then
            W_List_Id (List_Id (N));
         end if;
      end if;

      N_Indents := N_Indents - 1;
   end W_Node_Attribute;

   -------------------
   -- W_Node_Header --
   -------------------

   procedure W_Node_Header (N : Node_Id) is
   begin
      W_Indents;
      Write_Int  (Int (N));
      Write_Char (' ');
      Write_Str  (Image (Kind (N)));
      Write_Char (' ');
      Write_Line (Image (Loc (N)));
   end W_Node_Header;

   ---------------
   -- W_Node_Id --
   ---------------

   procedure W_Node_Id (N : Node_Id) is
   begin
      if N = No_Node then
         return;
      end if;
      W_Node (N);
   end W_Node_Id;

   ---------
   -- wfi --
   ---------

   procedure wfi (N : Node_Id) is
      I : constant Natural := N_Indents;
   begin
      N_Indents := 1;
      W_Node_Id (N);
      N_Indents := I;
   end wfi;

end Frontend.Debug;
