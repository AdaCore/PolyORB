------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.REPRESENTATIONS.SOAP.NAME_SPACES                  --
--                                                                          --
--                                 S p e c                                  --
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




--  $Id$

with PolyORB.Representations.SOAP;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Objects;
with PolyORB.Requests;
with Sequences.Unbounded;

package PolyORB.Representations.SOAP.Name_Spaces is

   use PolyORB.Obj_Adapters.Simple;
   use PolyORB.Objects;

   NS_Free      : constant Integer := 0;
   Number_Of_NS : constant Integer := 50;
   subtype NS_Index is Integer range 0 .. Number_Of_NS;

   type Namespace_Object is private;
   type Namespace_Object_Access is access all Namespace_Object;

   package Namespace_Seq is new Sequences.Unbounded (Namespace_Object_Access);

   protected type Protected_Namespace_Table is
      procedure Allocate (data : in Namespace_Object_Access;
           idx : out NS_Index);
      procedure Deallocate (idx : in NS_Index);
      function  Instance (idx  : in NS_Index) return Namespace_Object_Access;
      function  Instance (urn  : in XML_String) return Namespace_Object_Access;
   private
      NS        : Namespace_Seq.Sequence  := Namespace_Seq.Null_Sequence;
   end Protected_Namespace_Table;

   NS_Table : Protected_Namespace_Table;

   procedure Add_Element
     (URN   : XML_String := XML_Null_String;
      Element : XML_Component_Access);

   function URN (This : Namespace_Object_Access)
       return XML_String;

   procedure Add_Name
     (Element  : XML_Component_Access;
      Urn    : XML_String);

   procedure Add_PolyORB_Method
     (OA     : access Simple_Obj_Adapter;
      Oid    : Object_Id;
      Method : Requests.Operation_Id;
      Urn    : XML_String);


private

   type Namespace_Object is record
     URN : XML_String := XML_Null_String;
     Names : Container_Element_Access := null;
     Reference : Integer := 0;
   end record;

end PolyORB.Representations.SOAP.Name_Spaces;
