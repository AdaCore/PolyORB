------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . A N Y . O B J R E F                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2002 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Any's that contain object references.

package body PolyORB.Any.ObjRef is

   --  'Object Reference' content

   type Content_ObjRef is new Content with record
      Value : PolyORB.References.Ref_Ptr;
   end record;

   type Content_ObjRef_Ptr is access all Content_ObjRef;

   procedure Deallocate (Object : access Content_ObjRef);

   function Duplicate
     (Object : access Content_ObjRef)
     return Any_Content_Ptr;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Object : access Content_ObjRef)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      PolyORB.References.Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
   end Deallocate;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate
     (Object : access Content_ObjRef)
     return Any_Content_Ptr is
   begin
      return new Content_ObjRef'
        (Value => new PolyORB.References.Ref'
         (Content_ObjRef_Ptr (Object).Value.all));
   end Duplicate;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : in PolyORB.References.Ref)
     return Any
   is
      Result : Any;
      Content : constant Any_Content_Ptr := new Content_ObjRef;
   begin
      Content_ObjRef (Content.all).Value
        := new PolyORB.References.Ref'(Item);

      Set_Value (Result, Content);
      Set_Type (Result, TypeCode.TC_Object);

      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : in Any)
     return PolyORB.References.Ref is
   begin
      if TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Objref then
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
      Value     : in     PolyORB.References.Ref)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));

   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Objref then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value /= null then
         Content_ObjRef_Ptr (Container.The_Value).Value.all := Value;
      else
         Container.The_Value := new Content_ObjRef'
           (Value => new PolyORB.References.Ref'(Value));
      end if;
   end Set_Any_Value;

end PolyORB.Any.ObjRef;
