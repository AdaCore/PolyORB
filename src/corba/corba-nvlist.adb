--  $Id$

with Ada.Tags;

with Droopi.Log;
with Droopi.Locks; use Droopi.Locks;
with Droopi.Smart_Pointers;

package body CORBA.NVList is

   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("corba.nvlist");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Obj : in out Object) is
   begin
      pragma Debug (O ("Finalize: enter"));
      NV_Sequence.Delete (Obj.List, 1, NV_Sequence.Length (Obj.List));
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
      Obj : constant Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      return CORBA.Long (NV_Sequence.Length (Obj.List));
   end Get_Count;

   ------------
   -- Create --
   ------------

   procedure Create (NVList : out Ref)
   is
      Object : constant Object_Ptr := Create_Object;
   begin
      Set (NVList, Droopi.Smart_Pointers.Entity_Ptr (Object));
   end Create;

   function Image (NVList : Ref) return Standard.String
   is
      use NV_Sequence;

      Obj : constant Object_Ptr := Object_Ptr (Object_Of (NVList));
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Obj /= null then
         declare
            NVs : constant Element_Array := To_Element_Array (Obj.List);
         begin
            for I in NVs'Range loop
               Ada.Strings.Unbounded.Append (Result, Image (NVs (I)));
            end loop;

            return Ada.Strings.Unbounded.To_String (Result);
         end;
      else
         return ("(null list)");
      end if;
   end Image;

--     --------------
--     -- Marshall --
--     --------------
--
--     procedure Marshall
--       (Buffer : access Droopi.Buffers.Buffer_Type;
--        Data   : Ref) is
--        Actual_Ref : constant Object_Ptr
--          := Object_Ptr (Object_Of (Data));
--        List : NV_Sequence.Element_Array :=
--          NV_Sequence.To_Element_Array (Actual_Ref.List);
--     begin
--        pragma Debug (O ("Marshall : enter"));
--        for Index in List'Range loop
--           pragma Debug (O ("Marshall : dealing with a named value"));
--           if List (Index).Arg_Modes = CORBA.ARG_IN or
--             List (Index).Arg_Modes = CORBA.ARG_INOUT then
--              pragma Debug (O ("Marshall : marshalling a named value"));
--              pragma Debug
--                (O ("Marshall : NV type is "
--                    & Ada.Tags.External_Tag
--                    (Get_Value (List (Index).Argument).all'Tag)));
--              Droopi.CDR.Marshall (Buffer, List (Index));
--           end if;
--        end loop;
--        pragma Debug (O ("Marshall : end"));
--     end Marshall;
--
--     ----------------
--     -- Unmarshall --
--     ----------------
--
--     procedure Unmarshall
--       (Buffer : access Droopi.Buffers.Buffer_Type;
--        Data : in out Ref)
--     is
--        Actual_Ref : constant Object_Ptr
--          := Object_Ptr (Object_Of (Data));
--        List : NV_Sequence.Element_Array :=
--          NV_Sequence.To_Element_Array (Actual_Ref.List);
--     begin
--        pragma Debug (O ("Unmarshall : enter"));
--        for Index in List'Range loop
--           if List (Index).Arg_Modes = CORBA.ARG_OUT or
--             List (Index).Arg_Modes = CORBA.ARG_INOUT then
--              pragma Debug (O
--                ("Unmarshall : about to unmarshall a NamedValue"));
--              pragma Debug (O ("Unmarshall : is_empty := "
--                               & Boolean'Image (CORBA.Is_Empty
--                                   (List (Index).Argument))));
--              Droopi.CDR.Unmarshall (Buffer, List (Index));
--              pragma Debug (O ("Unmarshall : is_empty := "
--                               & Boolean'Image (CORBA.Is_Empty
--                                                (List (Index).Argument))));
--              pragma Debug (O ("Unmarshall : kind is "
--                               & CORBA.TCKind'Image
--                               (CORBA.TypeCode.Kind
--                                (CORBA.Get_Type (List (Index).Argument)))));
--              pragma Debug (O ("Unmarshall : value kind is "
--                               & Ada.Tags.External_Tag
--                                (Get_Value (List
--                                  (Index).Argument).all'Tag)));
--           end if;
--        end loop;
--        pragma Debug (O ("Unmarshall : end"));
--     end Unmarshall;

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
