------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . N V L I S T                          --
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

--  $Id$

with System.Address_To_Access_Conversions;

package body CORBA.NVList is

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item       : in CORBA.Any;
      Item_Flags : in Flags)
   is
   begin
      PolyORB.Any.NVList.Add_Item
        (To_PolyORB_Ref (Self),
         PolyORB.Types.Identifier (Item_Name),
         Item, Item_Flags);
   end Add_Item;

   procedure Add_Item
     (Self : Ref;
      Item : in CORBA.NamedValue)
   is
      --  PItem : PolyORB.Any.NamedValue;
      --  for PItem'Address use Item'Address;
      --  pragma Import (Ada, PItem);
      package PAAC is new System.Address_To_Access_Conversions
        (PolyORB.Any.NamedValue);
      --  Ugly but necessary; see comments in
      --  CORBA.Request.Create_Request.
   begin
      --  PolyORB.Any.NVList.Add_Item
      --    (To_PolyORB_Ref (Self), PItem);
      PolyORB.Any.NVList.Add_Item
        (To_PolyORB_Ref (Self),
         PAAC.To_Pointer (Item'Address).all);
   end Add_Item;

   function Get_Count (Self : Ref) return CORBA.Long is
   begin
      return CORBA.Long (PolyORB.Any.NVList.Get_Count (To_PolyORB_Ref (Self)));
   end Get_Count;

   procedure Free (Self : Ref)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      null;
   end Free;

   function To_PolyORB_Ref (Self : Ref) return PolyORB.Any.NVList.Ref
   is
      Res : PolyORB.Any.NVList.Ref;
   begin
      PolyORB.Any.NVList.Set (Res, Entity_Of (Self));
      return Res;
   end To_PolyORB_Ref;

   function To_CORBA_Ref (Self : PolyORB.Any.NVList.Ref) return Ref
   is
      Res : Ref;
   begin
      Set (Res, PolyORB.Any.NVList.Entity_Of (Self));
      return Res;
   end To_CORBA_Ref;

   procedure Create (Self : out Ref)
   is
      Res : PolyORB.Any.NVList.Ref;
   begin
      PolyORB.Any.NVList.Create (Res);
      Self := To_CORBA_Ref (Res);
   end Create;

   function Item (Self : Ref; Index : CORBA.Long)
     return CORBA.NamedValue
   is
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Sequence;
   begin
      return
        To_CORBA_NV
        (Element_Of
         (List_Of (To_PolyORB_Ref (Self)).all, Integer (Index)));
   end Item;

end CORBA.NVList;
