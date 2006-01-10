------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . S O A P _ P . M E S S A G E . R E A D E R         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2006, Free Software Foundation, Inc.          --
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
