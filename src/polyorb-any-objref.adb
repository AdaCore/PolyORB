--  PolyORB components

--  $Id$

--  Any's that contain object references.

with PolyORB.Locks;

package body PolyORB.Any.ObjRef is

   use PolyORB.Locks;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in PolyORB.References.Ref) return Any
   is
      Result : Any;
   begin
      Set_Value
        (Result, new Content_ObjRef'
         (Content with Value => new PolyORB.References.Ref'(Item)));
      Set_Type (Result, TypeCode.TC_Object);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in Any) return PolyORB.References.Ref
   is
   begin
      if (TypeCode.Kind (Get_Type (Item)) /= Tk_Objref) then
         raise TypeCode.Bad_TypeCode;
      end if;

      return PolyORB.References.Ref
        (Content_ObjRef_Ptr (Get_Value (Item)).Value.all);
   end From_Any;

   ---------------------
   --  Set_Any_Value  --
   ---------------------

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value : in PolyORB.References.Ref)
   is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Precise_Type (Any_Value)) /= Tk_Objref
      then
         raise TypeCode.Bad_TypeCode;
      end if;

      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= Null_Content_Ptr then
         Content_ObjRef_Ptr (Any_Value.The_Value.all).Value.all
           := PolyORB.References.Ref (Value);
      else
         Any_Value.The_Value.all := new Content_ObjRef'
           (Value => new PolyORB.References.Ref'(Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

end PolyORB.Any.ObjRef;
