------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         C O R B A . N V L I S T                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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

with Ada.Tags;

with Broca.CDR;
with Broca.Debug;
with Broca.Locks; use Broca.Locks;

package body CORBA.NVList is

   Flag : constant Natural
     := Broca.Debug.Is_Active ("corba.nvlist");
   procedure O is new Broca.Debug.Output (Flag);

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Obj : in out Object) is
   begin
      pragma Debug (O ("Finalize: enter"));
      NV_Sequence.Delete (Obj.List,
                          1,
                          NV_Sequence.Length (Obj.List));
      pragma Debug (O ("Finalize: end"));
   end Finalize;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item       : in CORBA.Any;
      Item_Flags : in Flags) is
      Argument :  CORBA.Any;
      The_Value : Any_Content_Ptr_Ptr;
      The_Counter : Natural_Ptr;
   begin
      pragma Debug (O ("Add_Item (4 params) : enter"));
      pragma Debug (O ("Add_Item (4 params) : Item type is "
                       & Ada.Tags.External_Tag (Get_Value (Item).all'Tag)));
      pragma Debug (O ("Add_Item (4 params) : ref_counter = "
                       & Positive'Image (Get_Counter (Item).all)));
      Lock_W (Item.Any_Lock);
      The_Value := Item.The_Value;
      The_Counter := Item.Ref_Counter;
      Item.Ref_Counter.all := Item.Ref_Counter.all + 1;
      Unlock_W (Item.Any_Lock);
      Argument := (Ada.Finalization.Controlled with
                   The_Value => The_Value,
                   The_Type => Item.The_Type,
                   As_Reference => True,
                   Ref_Counter => The_Counter,
                   Any_Lock => Item.Any_Lock);
      Add_Item (Self, (Name => Item_Name,
                       Argument => Argument,
                       Arg_Modes => Item_Flags));
      pragma Debug (O ("Add_Item (4 params) : ref_counter = "
                       & Positive'Image (Get_Counter (Item).all)));
      pragma Debug (O ("Add_Item (4 params) : end"));
   end Add_Item;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Self : Ref;
      Item : NamedValue)
   is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      pragma Debug (O ("Add_Item (2 params) : enter"));
      pragma Debug (O ("Add_Item (2 params) : ref_counter = "
                       & Positive'Image (Item.Argument.Ref_Counter.all)));
      NV_Sequence.Append (Obj.List, Item);
      pragma Debug (O ("Add_Item (2 params) : ref_counter = "
                       & Positive'Image (Item.Argument.Ref_Counter.all)));
      pragma Debug (O ("Add_Item (2 params) : end"));
   end Add_Item;

   ----------
   -- Free --
   ----------

   procedure Free (Self : Ref) is
   begin
      null;
   end Free;

   ---------------
   -- Get_Count --
   ---------------

   function Get_Count (Self : Ref) return CORBA.Long
   is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      return CORBA.Long (NV_Sequence.Length (Obj.List));
   end Get_Count;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Broca.Buffers.Buffer_Type;
      Data   : Ref) is
      Actual_Ref : constant Object_Ptr
        := Object_Ptr (Object_Of (Data));
      List : NV_Sequence.Element_Array :=
        NV_Sequence.To_Element_Array (Actual_Ref.List);
   begin
      pragma Debug (O ("Marshall : enter"));
      for Index in List'Range loop
         pragma Debug (O ("Marshall : dealing with a named value"));
         if List (Index).Arg_Modes = CORBA.ARG_IN or
           List (Index).Arg_Modes = CORBA.ARG_INOUT then
            pragma Debug (O ("Marshall : marshalling a named value"));
            pragma Debug
              (O ("Marshall : NV type is "
                  & Ada.Tags.External_Tag
                  (Get_Value (List (Index).Argument).all'Tag)));
            Broca.CDR.Marshall (Buffer, List (Index));
         end if;
      end loop;
      pragma Debug (O ("Marshall : end"));
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : access Broca.Buffers.Buffer_Type;
      Data : in out Ref)
   is
      Actual_Ref : constant Object_Ptr
        := Object_Ptr (Object_Of (Data));
      List : NV_Sequence.Element_Array :=
        NV_Sequence.To_Element_Array (Actual_Ref.List);
   begin
      pragma Debug (O ("Unmarshall : enter"));
      for Index in List'Range loop
         if List (Index).Arg_Modes = CORBA.ARG_OUT or
           List (Index).Arg_Modes = CORBA.ARG_INOUT then
            pragma Debug (O ("Unmarshall : about to unmarshall a NamedValue"));
            pragma Debug (O ("Unmarshall : is_empty := "
                             & Boolean'Image (CORBA.Is_Empty
                                              (List (Index).Argument))));
            Broca.CDR.Unmarshall (Buffer, List (Index));
            pragma Debug (O ("Unmarshall : is_empty := "
                             & Boolean'Image (CORBA.Is_Empty
                                              (List (Index).Argument))));
            pragma Debug (O ("Unmarshall : kind is "
                             & CORBA.TCKind'Image
                             (CORBA.TypeCode.Kind
                              (CORBA.Get_Type (List (Index).Argument)))));
            pragma Debug (O ("Unmarshall : value kind is "
                             & Ada.Tags.External_Tag
                              (Get_Value (List (Index).Argument).all'Tag)));
         end if;
      end loop;
      pragma Debug (O ("Unmarshall : end"));
   end Unmarshall;

   -------------------
   -- Create_Object --
   -------------------
   function Create_Object return Object_Ptr
   is
      Actual_Ref : constant CORBA.NVList.Object_Ptr
        := new Object;
   begin
      Actual_Ref.List := NV_Sequence.Null_Sequence;
      return Actual_Ref;
   end Create_Object;

end CORBA.NVList;
