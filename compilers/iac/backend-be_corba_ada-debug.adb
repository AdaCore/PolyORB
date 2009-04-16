------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           B A C K E N D . B E _ C O R B A _ A D A . D E B U G            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
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
with Lexer;       use Lexer;
with Namet;       use Namet;
with Scopes;
with Utils;       use Utils;
with Values;      use Values;

with Frontend.Nodes;

package body Backend.BE_CORBA_Ada.Debug is

   package FEN renames Frontend.Nodes;

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
         return No_Str;
      else
         return Get_Name_String (N);
      end if;
   end Image;

   function Image (N : Value_Id) return String is
   begin
      return Values.Image (N);
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
      return Quoted (Image (Token_Type'Val (N)));
   end Image;

   function Image (N : Operator_Id) return String is
   begin
      return Quoted (Image (Operator_Type'Val (N)));
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
      N : Node_Id := Stub_Node (FEN.BE_Node (Scopes.IDL_Spec));
   begin
      N_Indents := 0;
      while Present (N) loop
         W_Node_Id (N);
         N := Next_Node (N);
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
      N : Node_Id;
   begin
      if L = No_List then
         return;
      end if;

      N := First_Node (L);
      while Present (N) loop
         W_Node_Id (N);
         N := Next_Node (N);
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
      --  Some fields must not be printed be cause they constitute
      --  redundant information.

      if A = "Next_Entity"
        or else A = "Next_Node"
        or else A = "Package_Declaration"
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
      elsif K = "Node_Id"
        and then Present (C)
      then
         case Kind (C) is
            when K_Float .. K_Any =>
               Write_Line ('(' & Image (Kind (Node_Id (N))) & ')');
            when others =>
               Write_Line (V);
         end case;
      else
         Write_Line (V);
      end if;

      --  Some fields must not be visited to avoid endless recursions

      if A /= "Node"
        and then A /= "Declaration_Node"
        and then A /= "FE_Node"
        and then A /= "Parent"
        and then A /= "IDL_Unit"
        and then A /= "Designated_Node"
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

   ----------
   -- wabi --
   ----------

   procedure wabi (N : Node_Id) is
      I : constant Natural := N_Indents;
   begin
      N_Indents := 1;
      W_Node_Id (N);
      N_Indents := I;
   end wabi;

end Backend.BE_CORBA_Ada.Debug;
