------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . S O A P _ P . M E S S A G E . R E A D E R         --
--                                                                          --
--                                 S p e c                                  --
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

--  This package is based on Tree_Reader from the XMLada package. It is used
--  to create a DOM object using the SAX parser.

with Sax.Readers;          use Sax.Readers;
with Sax.Attributes;
with Unicode.CES;
with DOM.Core;             use DOM.Core;

private package PolyORB.SOAP_P.Message.Reader is

   type Tree_Reader is new Sax.Readers.Reader with private;
   --  Tree_Reader create a DOM tree using the SAX parser.

   function Get_Tree (Read : Tree_Reader) return Document;

private

   type Tree_Reader is new Sax.Readers.Reader with record
      Tree              : Document;
      Current_Node      : Node;
      Internal_Encoding : Unicode.CES.Encoding_Scheme;
   end record;

   procedure Start_Document
     (Handler : in out Tree_Reader);

   procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence       := "";
      Local_Name    : Unicode.CES.Byte_Sequence       := "";
      Qname         : Unicode.CES.Byte_Sequence       := "";
      Atts          : Sax.Attributes.Attributes'Class);

   procedure End_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");

   procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : Unicode.CES.Byte_Sequence);

   procedure Ignorable_Whitespace
     (Handler : in out Tree_Reader;
      Ch      : Unicode.CES.Byte_Sequence);

end PolyORB.SOAP_P.Message.Reader;
