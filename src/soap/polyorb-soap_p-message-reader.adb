------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . S O A P _ P . M E S S A G E . R E A D E R         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  This package is based on Tree_Reader from the XMLada package.

with Sax.Attributes;       use Sax.Attributes;
with Unicode;              use Unicode;
with Unicode.CES;          use Unicode.CES;
with DOM.Core.Nodes;       use DOM.Core.Nodes;
with DOM.Core.Documents;   use DOM.Core.Documents;
with DOM.Core.Elements;    use DOM.Core.Elements;

package body PolyORB.SOAP_P.Message.Reader is

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off);
      Tmp : constant Node := Append_Child
        (Handler.Current_Node, Create_Text_Node (Handler.Tree, Ch));
      pragma Unreferenced (Tmp);
      pragma Warnings (On);
   begin
      null;
   end Characters;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
      pragma Warnings (Off);
      pragma Unreferenced
        (Namespace_URI,
         Local_Name,
         Qname);
      pragma Warnings (On);
   begin
      Handler.Current_Node := Parent_Node (Handler.Current_Node);
   end End_Element;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree (Read : Tree_Reader) return Document is
   begin
      return Read.Tree;
   end Get_Tree;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   procedure Ignorable_Whitespace
     (Handler : in out Tree_Reader;
      Ch      : Unicode.CES.Byte_Sequence)
   is
   begin
      --  Ignore these white spaces at the toplevel
      if Ch'Length = 1
        and then Ch (Ch'First) /= ASCII.LF
        and then Handler.Current_Node /= Handler.Tree
      then
         declare
            pragma Warnings (Off);
            Tmp : constant Node := Append_Child
              (Handler.Current_Node, Create_Text_Node (Handler.Tree, Ch));
            pragma Unreferenced (Tmp);
            pragma Warnings (On);
         begin
            null;
         end;
      end if;
   end Ignorable_Whitespace;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Tree_Reader) is
      Implementation : DOM_Implementation;
   begin
      Handler.Tree := Create_Document (Implementation);
      Handler.Current_Node := Handler.Tree;
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence       := "";
      Local_Name    : Unicode.CES.Byte_Sequence       := "";
      Qname         : Unicode.CES.Byte_Sequence       := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Local_Name);
      pragma Warnings (On);
   begin
      Handler.Current_Node := Append_Child
        (Handler.Current_Node,
         Create_Element_NS (Handler.Tree,
                            Namespace_URI => Namespace_URI,
                            Qualified_Name => Qname));

      --  Insert the attributes in the right order.
      for J in 0 .. Get_Length (Atts) - 1 loop
         Set_Attribute_NS
           (Handler.Current_Node,
            Get_URI (Atts, J),
            Get_Qname (Atts, J),
            Get_Value (Atts, J));
      end loop;
   end Start_Element;

end PolyORB.SOAP_P.Message.Reader;
