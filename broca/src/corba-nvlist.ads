------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        C O R B A . N V L I S T                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Object;
with System;
with Ada.Unchecked_Deallocation;

package CORBA.NVList is

   type Ref is new CORBA.Object.Ref with null record;

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item_Type  : in CORBA.TypeCode.Object;
      Value      : in System.Address;
      Len        : in Long;
      Item_Flags : in Flags);

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item       : in CORBA.Any;
      Item_Flags : in Flags);

   procedure Add_Item
     (Self : Ref;
      Item : CORBA.NamedValue);

   --  free and free_memory not needed in Ada
   procedure Free (Self : Ref);
   procedure Free_Memory (Self : Ref);

   function Get_Count (Self : Ref) return CORBA.Long;

private
   --  the actual implementation of an NVList, just a list of NamedValue
   type NV_Cell;
   type NV_List is access all NV_Cell;
   type NV_Cell is record
      NV : NamedValue;
      Next : NV_List;
   end record;
   Null_NVList : constant NV_List := null;

   --  some usefull fonctions - no further comment are needed
   procedure Add_Cell (List : in out NV_List;
                       NV : in NamedValue);

   procedure Free is new Ada.Unchecked_Deallocation (NV_Cell, NV_List);
   procedure Free_All_List (List : in out NV_List);

   function Length (List : in NV_List) return CORBA.Long;


   --  in order to be able to deal with several NVLists at the
   --  same time, each NVList is associated to a Ref.
   --  The set of NVLists is stored in an NVList_List
   type NVList_Cell;
   type NVList_List is access all NVList_Cell;
   type NVList_Cell is record
      Obj : Ref;
      List : NV_List;
      Next : NVList_List;
   end record;
   Null_NVList_List : constant NVList_List := null;

   --  Adds a cell to the list of current NVLists
   --  The new Cell will be associated to List
   procedure Add_NVList (Obj : in Ref;
                         List : in NV_List);

   --  removes an NV_List from the list and frees it
   --  does nothing if the given Ref does not correspond to any list
   procedure Remove_NVList (Obj : Ref);

   --  returns the list associated to a given Ref
   --  returns Null_NVList_List if the given Ref does not correspond
   --  to any list
   function Get_NVList (Obj : Ref) return NV_List;

   --  to free a NV_List_Cell
   procedure Free is new Ada.Unchecked_Deallocation
     (NVList_Cell, NVList_List);

end CORBA.NVList;
