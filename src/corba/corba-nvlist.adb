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
        (Self.Self,
         Droopi.Types.Identifier (Item_Name),
         Item, Item_Flags);
   end Add_Item;

   procedure Add_Item
     (Self : Ref;
      Item : in CORBA.NamedValue) is
   begin
      Droopi.Any.NVList.Add_Item (Self.Self, Item);
   end Add_Item;

   function Get_Count (Self : Ref) return CORBA.Long is
   begin
      return CORBA.Long (Droopi.Any.NVList.Get_Count (Self.Self));
   end Get_Count;

   procedure Free (Self : Ref) is
   begin
      null;
   end Free;

   function To_Droopi_Ref (Self : Ref) return Droopi.Any.NVList.Ref is
   begin
      return Self.Self;
   end To_Droopi_Ref;

   function To_CORBA_Ref (Self : Droopi.Any.NVList.Ref) return Ref is
   begin
      return (CORBA.AbstractBase.Ref with Self => Self);
   end To_CORBA_Ref;

   procedure Create (Self : out Ref)
   is
   begin
      Droopi.Any.NVList.Create (Self.Self);
   end Create;

end CORBA.NVList;
