------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                         G L A D E . B U F F E R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body GLADE.Buffer is

   type Node;
   type Node_Access is access Node;

   type Node is record
      Content : Element_Type;
      Next    : Node_Access;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

   protected FIFO is
      procedure Put (Element : in Element_Type);
      entry Get (Element : out Element_Type);
      function Count return Natural;
   private
      First, Last : Node_Access;
      Element_Count : Natural := 0;
   end FIFO;

   -----------
   -- Count --
   -----------

   function Count return Natural is
   begin
      return FIFO.Count;
   end Count;

   ----------
   -- FIFO --
   ----------

   protected body FIFO is

      -----------
      -- Count --
      -----------

      function Count return Natural is
      begin
         return Element_Count;
      end Count;

      ---------
      -- Get --
      ---------

      entry Get (Element : out Element_Type) when Element_Count > 0
      is
         Next : constant Node_Access := First.Next;
      begin
         Element := First.Content;
         Free (First);
         First := Next;
         if First = null then
            Last := null;
         end if;
         Element_Count := Element_Count - 1;
      end Get;

      ---------
      -- Put --
      ---------

      procedure Put (Element : in Element_Type)
      is
         New_Node : constant Node_Access := new Node'(Element, null);
      begin
         if Last = null then
            First := New_Node;
            Last  := New_Node;
         else
            Last.Next := New_Node;
            Last := New_Node;
         end if;
         Element_Count := Element_Count + 1;
      end Put;

   end FIFO;

   ---------
   -- Get --
   ---------

   function Get return Element_Type
   is
      Result : Element_Type;
   begin
      FIFO.Get (Result);
      return Result;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (Element : in Element_Type) is
   begin
      FIFO.Put (Element);
   end Put;

end GLADE.Buffer;
