------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.REPRESENTATIONS.SOAP.NAME_SPACES                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Representations.SOAP;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Requests;

with PolyORB.Protocols.SOAP;

with Sequences.Unbounded;

package body PolyORB.Representations.SOAP.Name_Spaces is

   use Namespace_Seq;

   protected body Protected_Namespace_Table is

      procedure Allocate
        (data : in Namespace_Object_Access;
         idx  : out NS_Index)
      is
      begin
         if Length (NS) <= Number_Of_NS then
            Append (NS, data);
            idx := Length (NS);
         else
            raise SOAP_Error;
         end if;
      end Allocate;

      procedure Deallocate (idx : in NS_Index)
      is
      begin
         NS := Delete (NS, idx, 1);
      end Deallocate;

      function  Instance (idx  : in NS_Index)
            return Namespace_Object_Access
      is
      begin
         return Element_Of (NS, idx);
      end Instance;


      function Instance (urn  : in XML_String)
              return Namespace_Object_Access
      is
      begin
         for I in 1 .. Length (NS) loop
            if Element_Of (NS, I).URN = urn then
               return  Element_Of (NS, I);
            end if;
         end loop;
         return null;
      end Instance;

   end Protected_Namespace_Table;

   procedure Add_Element
     (URN   : XML_String := XML_Null_String;
      Element : XML_Component_Access)
   is
      Namespace_Obj : Namespace_Object_Access :=
               NS_Table.Instance (URN);
      Ind : NS_Index;
   begin
      if Namespace_Obj = null then
         declare
            Obj : constant Namespace_Object_Access
              := new Namespace_Object'
              (URN => URN, Names => null, Reference => 0);
            Elt : constant Child_List_Access
              := new Child_List_Record'
              (Item => Element, Next => null);
            Cont : constant Container_Element_Access
              := new Container_Element'
              (Nbr_Of_Items => 0, Head => Elt, Tail => Elt);
         begin
            Obj.Names := Cont;
            NS_Table.Allocate (Obj, Ind);
         end;
      else
         Add_Comp_List (Namespace_Obj.Names, Element);
      end if;
   end Add_Element;

   function URN (This : Namespace_Object_Access)
       return XML_String
   is
   begin
      return This.URN;
   end URN;


   procedure Add_Name
     (Element  : XML_Component_Access;
      Urn    : XML_String)
   is
      Container : Container_Element_Access
                  := new Container_Element;
      Current_Elt : Child_List_Access;
   begin

      Tree_Course (Element, Container);

      if Container /= null then
         Current_Elt := Container.Head;
         if Element.Is_Method = True then
            declare
               Obj : Namespace_Object_Access := new Namespace_Object;
               Ind : NS_Index;
            begin
               Obj.URN := Urn;
               NS_Table.Allocate (Obj, Ind);
               Add_Element (Urn, Current_Elt.Item);
            end;
         else
            raise SOAP_Error;
         end if;
         loop
            exit when Current_Elt = null;
            Current_Elt := Current_Elt.Next;
            Add_Element (Urn, Current_Elt.Item);
         end loop;
      end if;
   end Add_Name;


   procedure Add_PolyORB_Method
     (OA     : access Simple_Obj_Adapter;
      Oid    : Object_Id;
      Method : Requests.Operation_Id;
      Urn    : XML_String)
   is
      use PolyORB.Obj_Adapters.Simple;
      use PolyORB.Protocols.SOAP;
      XML_Comp : XML_Component_Access;
      Args : Any.NVList.Ref;
   begin
      Args := Get_Empty_Arg_List (OA, Oid, Method);
      Request_To_SOAP_Method (To_PolyORB_String (Method), Args, Urn, XML_Comp);
      Add_Name (XML_Comp, Urn);
   end Add_PolyORB_Method;


end PolyORB.Representations.SOAP.Name_Spaces;
