--  $Id$

with Droopi.Types;

package body CORBA.NVList is

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item       : in CORBA.Any;
      Item_Flags : in Flags) is
   begin
      Droopi.Any.NVList.Add_Item
        (To_Droopi_Ref (Self),
         Droopi.Types.Identifier (Item_Name),
         Item, Item_Flags);
   end Add_Item;

   procedure Add_Item
     (Self : Ref;
      Item : in CORBA.NamedValue) is
   begin
      Droopi.Any.NVList.Add_Item (To_Droopi_Ref (Self), Item);
   end Add_Item;

   function Get_Count (Self : Ref) return CORBA.Long is
   begin
      return CORBA.Long (Droopi.Any.NVList.Get_Count (To_Droopi_Ref (Self)));
   end Get_Count;

   procedure Free (Self : Ref) is
   begin
      null;
   end Free;

   function To_Droopi_Ref (Self : Ref) return Droopi.Any.NVList.Ref
   is
      Res : Droopi.Any.NVList.Ref;
   begin
      Droopi.Any.NVList.Set (Res, Entity_Of (Self));
      return Res;
   end To_Droopi_Ref;

   function To_CORBA_Ref (Self : Droopi.Any.NVList.Ref) return Ref
   is
      Res : Ref;
   begin
      Set (Res, Droopi.Any.NVList.Entity_Of (Self));
      return Res;
   end To_CORBA_Ref;

   procedure Create (Self : out Ref)
   is
      Res : Droopi.Any.NVList.Ref;
   begin
      Droopi.Any.NVList.Create (Res);
      Self := To_CORBA_Ref (Res);
   end Create;

   function Item (Self : Ref; Index : CORBA.Long)
     return CORBA.NamedValue
   is
      use Droopi.Any.NVList.Internals;
      use Droopi.Any.NVList.Internals.NV_Sequence;
   begin
      return Element_Of (List_Of (To_Droopi_Ref (Self)).all,
                         Integer (Index));
   end Item;

end CORBA.NVList;
