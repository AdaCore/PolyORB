--  $Id$

with PolyORB.Types;

package body CORBA.NVList is

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item       : in CORBA.Any;
      Item_Flags : in Flags) is
   begin
      PolyORB.Any.NVList.Add_Item
        (To_PolyORB_Ref (Self),
         PolyORB.Types.Identifier (Item_Name),
         Item, Item_Flags);
   end Add_Item;

   procedure Add_Item
     (Self : Ref;
      Item : in CORBA.NamedValue) is
   begin
      PolyORB.Any.NVList.Add_Item (To_PolyORB_Ref (Self), Item);
   end Add_Item;

   function Get_Count (Self : Ref) return CORBA.Long is
   begin
      return CORBA.Long (PolyORB.Any.NVList.Get_Count (To_PolyORB_Ref (Self)));
   end Get_Count;

   procedure Free (Self : Ref) is
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
      return Element_Of (List_Of (To_PolyORB_Ref (Self)).all,
                         Integer (Index));
   end Item;

end CORBA.NVList;
