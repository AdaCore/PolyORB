--  $Id$

with CORBA.AbstractBase;
pragma Elaborate_All (CORBA.AbstractBase);

with Droopi.Any.NVList;

package CORBA.NVList is

   type Ref is new CORBA.AbstractBase.Ref with private;

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item       : in CORBA.Any;
      Item_Flags : in Flags);
   --  Create a NamedValue and add it to this NVList.

   procedure Add_Item
     (Self : Ref;
      Item : in CORBA.NamedValue);
   --  Add a NamedValue to this NVList.

   function Get_Count (Self : Ref) return CORBA.Long;
   --  Return the number of items in this NVList.

   procedure Free (Self : Ref);
   procedure Free_Memory (Self : Ref) renames Free;
   --  Free and Free_Memory are no-ops in Ada.

   -----------------------------------------
   -- The following is specific to DROOPI --
   -----------------------------------------

   procedure Create (Self : out Ref);
   --  XXX THIS MUST BE REPLACED BY AN OVERRIDING OF
   --  Initialize!
   --  Requiring users to call Create is in violation of the
   --  standard CORBA API.

   function Item (Self : Ref; Index : CORBA.Long)
     return CORBA.NamedValue;

   function To_Droopi_Ref (Self : Ref) return Droopi.Any.NVList.Ref;
   function To_CORBA_Ref (Self : Droopi.Any.NVList.Ref) return Ref;

private

   type Ref is new CORBA.AbstractBase.Ref with null record;

   pragma Inline (Add_Item);
   pragma Inline (Get_Count);
   pragma Inline (Free);
   pragma Inline (To_Droopi_Ref);
   pragma Inline (To_CORBA_Ref);
   pragma Inline (Create);

end CORBA.NVList;
