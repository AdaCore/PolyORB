------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E P R E S E N T A T I O N S . C D R           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2005;

with Ada.Streams;

with System.Address_Image;

with PolyORB.Any.ObjRef;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.Representations.CDR.Common;
with PolyORB.Utils.Buffers;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package body PolyORB.Representations.CDR is

   use Ada.Streams;

   use PolyORB.Any;
   use PolyORB.Any.TypeCode;
   use PolyORB.Buffers;
   use PolyORB.Errors;
   use PolyORB.Log;
   use PolyORB.Parameters;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.representations.cdr");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   type Registry_Item is record
      Major   : Octet;
      Minor   : Octet;
      Factory : CDR_Representation_Factory;
   end record;

   package Factory_Lists is new Utils.Chained_Lists (Registry_Item);

   Factory_Registry : Factory_Lists.List;

   Enable_Fast_Path : Boolean;
   --  If True, some aggregates are allowed to be marshalled in one lump of
   --  data (instead of element per element), if they have a suitable memory
   --  representation.

   function Fast_Path_Element_Size
     (El_TCK : TCKind) return Types.Unsigned_Long;
   --  For a type that is a suitable element type for fast path marshalling
   --  of a sequence or array, return the type size. Otherwise return 0.

   procedure Fast_Path_Get_Info
     (ACC                 : access Aggregate_Content'Class;
      TC                  : TypeCode.Object_Ptr;
      Buffer              : access Buffer_Type;
      Aggregate_Data      : out System.Address;
      Aggregate_Size      : out Stream_Element_Count;
      Aggregate_Alignment : out Alignment_Type);
   --  Obtain the data address, data length and CDR alignment to be used
   --  for fast path (un)marshalling of ACC, an aggregate of type TC, from/to
   --  Buffer.
   --  Note that Aggregate_Size and Aggregate_Alignment are set only when
   --  Aggregate_Data is not null.

   ------------------
   -- TypeCode Ids --
   ------------------

   --  Numerical value associated to TypeCodes, as defined in CDR

   TC_Null_Id               : constant PolyORB.Types.Unsigned_Long := 0;
   TC_Void_Id               : constant PolyORB.Types.Unsigned_Long := 1;
   TC_Short_Id              : constant PolyORB.Types.Unsigned_Long := 2;
   TC_Long_Id               : constant PolyORB.Types.Unsigned_Long := 3;
   TC_Unsigned_Short_Id     : constant PolyORB.Types.Unsigned_Long := 4;
   TC_Unsigned_Long_Id      : constant PolyORB.Types.Unsigned_Long := 5;
   TC_Float_Id              : constant PolyORB.Types.Unsigned_Long := 6;
   TC_Double_Id             : constant PolyORB.Types.Unsigned_Long := 7;
   TC_Boolean_Id            : constant PolyORB.Types.Unsigned_Long := 8;
   TC_Char_Id               : constant PolyORB.Types.Unsigned_Long := 9;
   TC_Octet_Id              : constant PolyORB.Types.Unsigned_Long := 10;
   TC_Any_Id                : constant PolyORB.Types.Unsigned_Long := 11;
   TC_TypeCode_Id           : constant PolyORB.Types.Unsigned_Long := 12;
   TC_Principal_Id          : constant PolyORB.Types.Unsigned_Long := 13;
   TC_Object_Id             : constant PolyORB.Types.Unsigned_Long := 14;
   TC_Struct_Id             : constant PolyORB.Types.Unsigned_Long := 15;
   TC_Union_Id              : constant PolyORB.Types.Unsigned_Long := 16;
   TC_Enum_Id               : constant PolyORB.Types.Unsigned_Long := 17;
   TC_String_Id             : constant PolyORB.Types.Unsigned_Long := 18;
   TC_Sequence_Id           : constant PolyORB.Types.Unsigned_Long := 19;
   TC_Array_Id              : constant PolyORB.Types.Unsigned_Long := 20;
   TC_Alias_Id              : constant PolyORB.Types.Unsigned_Long := 21;
   TC_Except_Id             : constant PolyORB.Types.Unsigned_Long := 22;
   TC_Long_Long_Id          : constant PolyORB.Types.Unsigned_Long := 23;
   TC_Unsigned_Long_Long_Id : constant PolyORB.Types.Unsigned_Long := 24;
   TC_Long_Double_Id        : constant PolyORB.Types.Unsigned_Long := 25;
   TC_Wchar_Id              : constant PolyORB.Types.Unsigned_Long := 26;
   TC_Wide_String_Id        : constant PolyORB.Types.Unsigned_Long := 27;
   TC_Fixed_Id              : constant PolyORB.Types.Unsigned_Long := 28;
   TC_Value_Id              : constant PolyORB.Types.Unsigned_Long := 29;
   TC_Valuebox_Id           : constant PolyORB.Types.Unsigned_Long := 30;
   TC_Native_Id             : constant PolyORB.Types.Unsigned_Long := 31;
   TC_Abstract_Interface_Id : constant PolyORB.Types.Unsigned_Long := 32;
   TC_Local_Interface_Id    : constant PolyORB.Types.Unsigned_Long := 33;
   TC_Component_Id          : constant PolyORB.Types.Unsigned_Long := 34;
   TC_Home_Id               : constant PolyORB.Types.Unsigned_Long := 35;
   TC_Event_Id              : constant PolyORB.Types.Unsigned_Long := 36;

   --  Additional value used in CDR to represent an indirect pointer to a
   --  previously marhsalled typecode.

   TC_Indirect              : constant PolyORB.Types.Unsigned_Long :=
     16#ffffffff#;

   --  Tags for ValueTypes marshalling

   Null_Tag  : constant PolyORB.Types.Unsigned_Long := 0;
   Value_Tag : constant PolyORB.Types.Unsigned_Long := 16#7fffff00#;

   -----------------------------
   -- Typecode map management --
   -----------------------------

   function Find_TC
     (Representation : access CDR_Representation'Class;
      Offset         : Types.Long) return TypeCode.Object_Ptr;
   --  Return recorded TC at the specified offset. Constraint_Error is raised
   --  if there is no TC at that offset in the map.

   procedure Start_TC
     (Representation : access CDR_Representation'Class;
      TC_Ref         : TypeCode.Object_Ptr;
      Offset         : Types.Long;
      Complex        : Boolean);
   --  Append a new typecode to the current typecode map.
   --  Offset is relative to the buffer where TC is being marshalled. If this
   --  is a nested TC (within an enclosing complex TC), this is the
   --  encapsulation buffer for the enclosing complex TC.
   --  Complex indicates whether this TC is itself a complex one, in which
   --  case the Current_Complex index in Representation is updated to point to
   --  the new entry.

   --  Implementation note: when unmarshalling a typecode, proper reference
   --  semantics on typecode objects are provided only after a parameter has
   --  been added to the typecode, as the parameters aggregate is allocated
   --  lazily.

   procedure End_TC
     (Representation : access CDR_Representation'Class;
      Complex        : Boolean);
   --  Note that processing of current typecode has been completed.
   --  If Complex is True, Representation.Current_Complex is restored to
   --  the index of the enclosing complex TC.

   --  When unmarshalling a TypeCode, Start_TC and End_TC must each be
   --  called exactly once, and if the typecode encloses any nested typecode,
   --  then the nested typecode must be unmarshalled after the Start_TC call
   --  and before the End_TC call.

   function To_Absolute_Offset
     (Representation  : access CDR_Representation'Class;
      Relative_Offset : Types.Long) return Types.Long;
   --  Convert the offset of a typecode relative to the innermost enclosing
   --  buffer (i.e. the innermost enclosing complex typecode) to an offset
   --  relative to the buffer containing the outermost complex typecode.

   function Image (E : TC_Map_Entry) return String;
   --  Return string representation of E, for debugging purposes

   ---------------------------
   -- Create_Representation --
   ---------------------------

   function Create_Representation
     (Major : Types.Octet;
      Minor : Types.Octet) return CDR_Representation_Access
   is
      use Factory_Lists;

      Iter   : Iterator := First (Factory_Registry);
      Result : CDR_Representation_Access;
   begin
      while not Last (Iter) loop
         if Value (Iter).Major = Major
           and then Value (Iter).Minor = Minor
         then
            Result := Value (Iter).Factory.all;
            return Result;
         end if;

         Next (Iter);
      end loop;

      return null;
   end Create_Representation;

   ------------
   -- End_TC --
   ------------

   procedure End_TC
     (Representation : access CDR_Representation'Class;
      Complex        : Boolean)
   is
      use TC_Maps;
      T : TC_Maps.Instance renames Representation.TC_Map;
   begin
      pragma Debug (C, O ("End_TC: Complex = " & Complex'Img
        & ", Current_Complex =" & Representation.Current_Complex'Img));

      if not Complex then
         return;
      end if;

      Representation.Current_Complex :=
        T.Table (Representation.Current_Complex).Enclosing_Complex;

      if Representation.Current_Complex = -1 then
         --  Outermost complex typecode completed

         pragma Debug (C, O ("End_TC: left outermost complex"));
         Deallocate (T);
      end if;
   end End_TC;

   ----------------------------
   -- Fast_Path_Element_Size --
   ----------------------------

   function Fast_Path_Element_Size
     (El_TCK : TCKind) return Types.Unsigned_Long is
   begin
      case El_TCK is
         when Tk_Char | Tk_Octet =>
            return 1;

         when
           Tk_Short  |
           Tk_Ushort =>
            return 2;

         when
           Tk_Long   |
           Tk_Ulong  =>
            return 4;

         when others =>
            return 0;
      end case;
   end Fast_Path_Element_Size;

   ------------------------
   -- Fast_Path_Get_Info --
   ------------------------

   procedure Fast_Path_Get_Info
     (ACC                 : access Aggregate_Content'Class;
      TC                  : TypeCode.Object_Ptr;
      Buffer              : access Buffer_Type;
      Aggregate_Data      : out System.Address;
      Aggregate_Size      : out Stream_Element_Count;
      Aggregate_Alignment : out Alignment_Type)
   is
      TCK : TCKind;

      Outermost : Boolean := True;
      El_TC     : TypeCode.Object_Ptr;
      El_Size   : Types.Unsigned_Long;
      El_Count  : Types.Unsigned_Long := 1;

   begin
      Aggregate_Data := System.Null_Address;

      if not Enable_Fast_Path then
         pragma Warnings (Off);
         --  OUT parameters Aggregate_Size and Aggregate_Element not set as
         --  they are meaningless when Aggregate_Data is null.
         return;
         pragma Warnings (On);
      end if;

      --  Unwind one sequence type code, or multiple nested array typecodes

      El_TC := Unwind_Typedefs (TC);
      loop
         TCK := TypeCode.Kind (El_TC);
         exit when not (TCK = Tk_Array
                          or else
                        (Outermost and then TCK = Tk_Sequence));

         if TCK = Tk_Sequence then
            --  Aggregate elements count for a sequence has one additional
            --  element corresponding to the sequence length, which is not
            --  part of the fast path data.

            El_Count := El_Count * Get_Aggregate_Count (ACC.all) - 1;
         else
            El_Count := El_Count * TypeCode.Length (El_TC);
         end if;

         El_TC := TypeCode.Content_Type (El_TC);
         Outermost := False;
      end loop;

      --  Here El_TC is the inner (non-array/sequence) component type

      El_Size := Fast_Path_Element_Size (TypeCode.Kind (El_TC));

      --  Case of element type that does not allow fast path

      if El_Size = 0 then
         return;

      --  Case of multi-byte elements, where the expected buffer endianness
      --  is not the host endianness: need to byte swap each element.

      elsif El_Size > 1 and then Endianness (Buffer) /= Host_Order then
         return;
      end if;

      Aggregate_Data      := Unchecked_Get_V (ACC);
      Aggregate_Alignment := Alignment_Of (Short_Short_Integer (El_Size));
      Aggregate_Size      := Stream_Element_Count (El_Count * El_Size);

      pragma Debug (C, O ("Fast_Path_Get_Info:"
        & Aggregate_Size'Img & " bytes ("
        & El_Count'Img & " elements) at "
        & System.Address_Image (Aggregate_Data)
        & ", align on " & Aggregate_Alignment'Img));
   end Fast_Path_Get_Info;

   -------------
   -- Find_TC --
   -------------

   function Find_TC
     (Representation : access CDR_Representation'Class;
      Offset         : Types.Long) return TypeCode.Object_Ptr
   is
      use TC_Maps;

      T   : TC_Maps.Instance renames Representation.TC_Map;
      Lo  : Types.Long := First (T);
      Hi  : Types.Long := Last (T);
      Cur : Types.Long;
   begin
      pragma Debug (C, O ("Find_TC: Offset =" & Offset'Img));

      --  Dichotomic search in map, assumes that entries are in ascending
      --  offset order.

      while Lo <= Hi loop
         Cur := (Lo + Hi) / 2;
         if T.Table (Cur).Offset = Offset then
            pragma Debug (C, O ("Find_TC: found at" & Cur'Img));
            return T.Table (Cur).TC_Ref;

         elsif T.Table (Cur).Offset < Offset then
            Lo := Cur + 1;
         else
            Hi := Cur - 1;
         end if;
      end loop;

      pragma Debug (C, O ("Find_TC: not found"));
      raise Constraint_Error;
   end Find_TC;

   -----------
   -- Image --
   -----------

   function Image (E : TC_Map_Entry) return String is
   begin
      return "TC@" & E.Offset'Img & ": "
        & System.Address_Image (E.TC_Ref.all'Address)
        & " encl =" & E.Enclosing_Complex'Img;
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Enable_Fast_Path :=
        Get_Conf ("cdr", "enable_fast_path", Default => True);
   end Initialize;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer         : access Buffer_Type;
      Representation : access CDR_Representation'Class;
      Data           : PolyORB.Any.Any)
   is
      E      : Errors.Error_Container;
      Data_C : Any_Container'Class renames Get_Container (Data).all;
   begin
      pragma Debug (C, O ("Marshall (Any): enter"));

      Marshall (Buffer, Representation, Get_Type_Obj (Data_C), E);
      pragma Assert (not Found (E));
      --  ??? should propagate error appropriately

      pragma Debug (C, O ("Marshall (Any): type marshalled"));
      Marshall_From_Any (Representation, Buffer, Data_C, E);
      pragma Assert (not Found (E));
      --  ??? should propagate error appropriately

      pragma Debug (C, O ("Marshall (Any): end"));
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      R      : access CDR_Representation'Class;
      Data   : TypeCode.Object_Ptr;
      Error  : in out Errors.Error_Container)
   is
      Complex_Buffer : Buffer_Access;
      Nb : PolyORB.Types.Unsigned_Long;
   begin
      pragma Debug (C, O ("Marshall (Typecode): enter, kind = " &
                       TCKind'Image (TypeCode.Kind (Data))));

      case TypeCode.Kind (Data) is

         when Tk_Null =>
            Marshall (Buffer, TC_Null_Id);

         when Tk_Void =>
            Marshall (Buffer, TC_Void_Id);

         when Tk_Short =>
            Marshall (Buffer, TC_Short_Id);

         when Tk_Long =>
            Marshall (Buffer, TC_Long_Id);

         when Tk_Ushort =>
            Marshall (Buffer, TC_Unsigned_Short_Id);

         when Tk_Ulong =>
            Marshall (Buffer, TC_Unsigned_Long_Id);

         when Tk_Float =>
            Marshall (Buffer, TC_Float_Id);

         when Tk_Double =>
            Marshall (Buffer, TC_Double_Id);

         when Tk_Boolean =>
            Marshall (Buffer, TC_Boolean_Id);

         when Tk_Char =>
            Marshall (Buffer, TC_Char_Id);

         when Tk_Octet =>
            Marshall (Buffer, TC_Octet_Id);

         when Tk_Any =>
            Marshall (Buffer, TC_Any_Id);

         when Tk_TypeCode =>
            Marshall (Buffer, TC_TypeCode_Id);

         when Tk_Principal =>
            Marshall (Buffer, TC_Principal_Id);

         when Tk_Objref =>
            Marshall (Buffer, TC_Object_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            pragma Debug (C, O ("Marshall (TypeCode): marshalling the id"));
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            pragma Debug (C, O ("Marshall (TypeCode): marshalling the name"));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Struct =>
            Marshall (Buffer, TC_Struct_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            pragma Debug (C, O ("Marshall (TypeCode): marshalling the id"));
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            pragma Debug (C, O ("Marshall (TypeCode): marshalling the name"));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));

            Nb := Any.TypeCode.Member_Count (Data);
            pragma Debug (C, O ("Marshall (TypeCode): " &
                             "marshalling" & Nb'Img & " members"));
            Marshall (Complex_Buffer, Nb);
            if Nb /= 0 then
               for J in 0 .. Nb - 1 loop
                  pragma Debug (C, O ("Marshall (TypeCode): marshalling"
                                   & " member #" & J'Img & ": "
                                   & To_Standard_String
                                       (TypeCode.Member_Name (Data, J))));
                  Marshall (Complex_Buffer,
                            TypeCode.Member_Name (Data, J));

                  pragma Debug
                    (C, O ("Marshall (TypeCode): marshalling "
                        & "member type: "
                        & TCKind'Image
                        (TypeCode.Kind
                         (TypeCode.Member_Type (Data, J)))));
                  Marshall (Complex_Buffer,
                            R,
                            TypeCode.Member_Type (Data, J),
                            Error);

                  exit when Found (Error);

                  pragma Debug (C, O ("Marshall (TypeCode): "
                                   & "member marshalled"));
               end loop;
            end if;

            pragma Debug (C, O ("Marshall: all members marshalled"));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Union =>
            Marshall (Buffer, TC_Union_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));
            Marshall
              (Complex_Buffer, R, TypeCode.Discriminator_Type (Data), Error);

            if Found (Error) then
               --  Do not proceed further in error case

               Nb := 0;

            else
               Marshall (Complex_Buffer, TypeCode.Default_Index (Data));
               Nb := Any.TypeCode.Member_Count (Data);
               Marshall (Complex_Buffer, Nb);
            end if;

            --  Need an explicit guard for the 0 case because Nb is a
            --  Types.Unsigned_Long, and Nb - 1 underflows in that case.

            if Nb /= 0 then
               for J in 0 .. Nb - 1 loop
                  Marshall_From_Any
                    (R,
                     Complex_Buffer,
                     TypeCode.Member_Label (Data, J).all,
                     Error);
                  exit when Found (Error);

                  Marshall
                    (Complex_Buffer,
                     TypeCode.Member_Name (Data, J));

                  Marshall
                    (Complex_Buffer,
                     R,
                     TypeCode.Member_Type (Data, J),
                     Error);
                  exit when Found (Error);
               end loop;
            end if;

            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Enum =>
            Marshall (Buffer, TC_Enum_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));

            Nb := TypeCode.Member_Count (Data);
            Marshall (Complex_Buffer, Nb);
            if Nb /= 0 then
               for J in 0 .. Nb - 1 loop
                  Marshall (Complex_Buffer,
                            Any.TypeCode.Member_Name (Data, J));
               end loop;
            end if;

            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_String =>
            Marshall (Buffer, TC_String_Id);
            Marshall (Buffer, TypeCode.Length (Data));

         when Tk_Sequence =>
            Marshall (Buffer, TC_Sequence_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      R,
                      TypeCode.Content_Type (Data),
                      Error);
            Marshall (Complex_Buffer, TypeCode.Length (Data));

            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Array =>
            Marshall (Buffer, TC_Array_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      R,
                      TypeCode.Content_Type (Data),
                      Error);
            Marshall (Complex_Buffer,
                      TypeCode.Length (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Alias =>
            Marshall (Buffer, TC_Alias_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));
            Marshall (Complex_Buffer,
                      R,
                      TypeCode.Content_Type (Data),
                      Error);
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Except =>
            Marshall (Buffer, TC_Except_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));
            Nb := TypeCode.Member_Count (Data);
            Marshall (Complex_Buffer, Nb);

            if Nb /= 0 then
               for J in 0 .. Nb - 1 loop
                  Marshall (Complex_Buffer,
                            Any.TypeCode.Member_Name (Data, J));
                  Marshall (Complex_Buffer,
                            R,
                            Any.TypeCode.Member_Type (Data, J),
                            Error);
                  exit when Found (Error);
               end loop;
            end if;

            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Longlong =>
            Marshall (Buffer, TC_Long_Long_Id);

         when Tk_Ulonglong =>
            Marshall (Buffer, TC_Unsigned_Long_Long_Id);

         when Tk_Longdouble =>
            Marshall (Buffer, TC_Long_Double_Id);

         when Tk_Widechar =>
            Marshall (Buffer, TC_Wchar_Id);

         when Tk_Wstring =>
            Marshall (Buffer, TC_Wide_String_Id);
            Marshall (Buffer, TypeCode.Length (Data));

         when Tk_Fixed =>
            Marshall (Buffer, TC_Fixed_Id);
            Marshall (Buffer, TypeCode.Fixed_Digits (Data));
            Marshall (Buffer, TypeCode.Fixed_Scale (Data));

         when Tk_Value =>
            Marshall (Buffer, TC_Value_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));

            Marshall
              (Complex_Buffer,
               TypeCode.Type_Modifier (Data));
            Marshall
              (Complex_Buffer,
               R,
               TypeCode.Concrete_Base_Type (Data),
               Error);

            if Found (Error) then
               --  Do not proceed further in error case
               Nb := 0;
            else
               Nb := TypeCode.Member_Count (Data);
            end if;

            Marshall (Complex_Buffer, Nb);
            if Nb /= 0 then
               for J in 0 .. Nb - 1 loop
                  Marshall
                    (Complex_Buffer,
                     TypeCode.Member_Name (Data, J));
                  Marshall
                    (Complex_Buffer,
                     R,
                     TypeCode.Member_Type (Data, J),
                     Error);
                  exit when Found (Error);
                  Marshall
                    (Complex_Buffer,
                     TypeCode.Member_Visibility (Data, J));
               end loop;
            end if;

            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Valuebox =>
            Marshall (Buffer, TC_Valuebox_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));
            Marshall (Complex_Buffer,
                      R,
                      TypeCode.Content_Type (Data),
                      Error);
            if not Found (Error) then
               Marshall (Buffer, Encapsulate (Complex_Buffer));
            end if;
            Release (Complex_Buffer);

         when Tk_Native =>
            Marshall (Buffer, TC_Native_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Abstract_Interface =>
            Marshall (Buffer, TC_Abstract_Interface_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Local_Interface =>
            Marshall (Buffer, TC_Local_Interface_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Component =>
            Marshall (Buffer, TC_Component_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Home =>
            Marshall (Buffer, TC_Home_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Event =>
            Marshall (Buffer, TC_Event_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer, Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer, Any.TypeCode.Name (Data));
            Marshall
              (Complex_Buffer,
               TypeCode.Type_Modifier (Data));
            Marshall
              (Complex_Buffer,
               R,
               TypeCode.Concrete_Base_Type (Data),
               Error);

            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Marshall
                       (Complex_Buffer,
                        TypeCode.Member_Name (Data, J));

                     Marshall
                       (Complex_Buffer,
                        R,
                        TypeCode.Member_Type (Data, J),
                        Error);
                     exit when Found (Error);

                     Marshall
                       (Complex_Buffer,
                        TypeCode.Member_Visibility (Data, J));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
      end case;
      pragma Debug (C, O ("Marshall (Typecode): end"));
   end Marshall;

   -----------------------
   -- Marshall_From_Any --
   -----------------------

   overriding procedure Marshall_From_Any
     (R      : access CDR_Representation;
      Buffer : not null access Buffer_Type;
      CData  : Any.Any_Container'Class;
      Error  : in out Errors.Error_Container)
   is
      Data_Type : constant TypeCode.Object_Ptr :=
        Any.Unwind_Typedefs (Get_Type_Obj (CData));

      procedure Marshall_Aggregate_Element
        (TC    : TypeCode.Object_Ptr;
         ACC   : access Aggregate_Content'Class;
         Index : Types.Unsigned_Long);
      --  Marshall the Index'th element for aggregate ACC, of type TC

      procedure Marshall_Aggregate_Element
        (TC    : TypeCode.Object_Ptr;
         ACC   : access Aggregate_Content'Class;
         Index : Types.Unsigned_Long)
      is
         El_M  : aliased Mechanism := By_Value;
         El_CC : aliased Content'Class :=
           Get_Aggregate_Element (ACC, TC, Index, El_M'Access);
         El_C : Any_Container;
      begin
         Set_Type (El_C, TC);
         Set_Value (El_C, El_CC'Unchecked_Access);
         Marshall_From_Any (R, Buffer, El_C, Error);
      end Marshall_Aggregate_Element;

      TCK : constant TCKind := TypeCode.Kind (Data_Type);

   begin
      pragma Debug (C, O ("Marshall_From_Any: enter"));
      pragma Debug (C, O ("Marshall_From_Any: CData = " & Image (CData)));

      case TCK is

         when Tk_Null | Tk_Void =>
            null;

         when Tk_Short =>
            Marshall (Buffer, PolyORB.Types.Short'(From_Any (CData)));

         when Tk_Long =>
            Marshall (Buffer, PolyORB.Types.Long'(From_Any (CData)));

         when Tk_Ushort =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Short'(From_Any (CData)));

         when Tk_Ulong =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(From_Any (CData)));

         when Tk_Float =>
            Marshall (Buffer, PolyORB.Types.Float'(From_Any (CData)));

         when Tk_Double =>
            Marshall (Buffer, PolyORB.Types.Double'(From_Any (CData)));

         when Tk_Boolean =>
            Marshall (Buffer, PolyORB.Types.Boolean'(From_Any (CData)));

         when Tk_Char =>
            Marshall
              (CDR_Representation'Class (R.all),
               Buffer,
               PolyORB.Types.Char'(From_Any (CData)),
               Error);

         when Tk_Octet =>
            Marshall (Buffer, PolyORB.Types.Octet'(From_Any (CData)));

         when Tk_Any =>
            Marshall
              (Buffer,
               CDR_Representation'Class (R.all)'Access,
               PolyORB.Any.Any'(From_Any (CData)));

         when Tk_TypeCode =>
            Marshall
              (Buffer,
               CDR_Representation'Class (R.all)'Access,
               Object_Of (TypeCode.Local_Ref'(From_Any (CData))),
               Error);

         when Tk_Principal =>
            --  FIXME: TBD
            raise Program_Error;

         when Tk_Objref =>
            Marshall (Buffer, PolyORB.Any.ObjRef.From_Any (CData));

         when Tk_Union =>

            declare
               ACC : Aggregate_Content'Class
                       renames Aggregate_Content'Class
                                 (Get_Value (CData).all);

               Label_TC : constant TypeCode.Object_Ptr :=
                 Any.TypeCode.Discriminator_Type (Data_Type);
               Label_M  : aliased Mechanism := By_Value;
               Label_CC : aliased Content'Class :=
                 Get_Aggregate_Element (ACC'Access, Label_TC, 0,
                                        Label_M'Access);
               Label_C  : Any_Container;
            begin
               Set_Type (Label_C, Label_TC);
               Set_Value (Label_C, Label_CC'Unchecked_Access);
               Marshall_From_Any (R, Buffer, Label_C, Error);
               if Found (Error) then
                  return;
               end if;

               pragma Debug (C, O ("Marshall_From_Any: "
                 & "union label marshalled, value:" & Image (Label_C)));

               declare
                  Member_TC : constant Any.TypeCode.Object_Ptr :=
                    Any.TypeCode.Member_Type_With_Label (Data_Type, Label_C);
               begin

                  --  Member_TC may be void in case there is no union member
                  --  for this label.

                  if Any.TypeCode.Kind (Member_TC) /= Tk_Void then
                     pragma Assert (Any.Get_Aggregate_Count (ACC) = 2);
                     Marshall_Aggregate_Element (Member_TC, ACC'Access, 1);

                  else
                     pragma Assert (Any.Get_Aggregate_Count (ACC) = 1);
                     pragma Debug (C, O ("Marshall_From_Any: "
                       & "union with no member for this label"));

                     null;
                  end if;
               end;

               if Found (Error) then
                  return;
               end if;

               pragma Debug
                 (C, O ("Marshall_From_Any: union member value marshalled"));
            end;

         when Tk_Enum =>
            Marshall_Aggregate_Element
              (TypeCode.PTC_Unsigned_Long'Access,
               Aggregate_Content'Class (Get_Value (CData).all)'Access,
               0);

         when Tk_String =>

            --  We need to call the From_Any variant returning Standard.String
            --  here to account for both the bounded and unbounded cases.

            Marshall
              (CDR_Representation'Class (R.all),
               Buffer,
               To_PolyORB_String (From_Any (CData)),
               Error);

         when Tk_Struct | Tk_Except | Tk_Sequence | Tk_Array | Tk_Fixed =>
            declare
               Nb    : Types.Unsigned_Long;
               El_TC : TypeCode.Object_Ptr;

               ACC : Aggregate_Content'Class renames
                 Aggregate_Content'Class (Get_Value (CData).all);
            begin
               --  Set Nb and El_TC

               case TCK is
                  when Tk_Struct | Tk_Except =>
                     Nb := TypeCode.Member_Count (Data_Type);
                     --  El_TC will be set once for each member, in the loop

                  when Tk_Array =>
                     Nb    := TypeCode.Length (Data_Type);
                     El_TC := Unwind_Typedefs
                                (TypeCode.Content_Type (Data_Type));

                  when Tk_Fixed =>
                     Nb    := (Types.Unsigned_Long
                               (TypeCode.Fixed_Digits (Data_Type)) + 2) / 2;
                     El_TC := TypeCode.PTC_Octet'Access;

                  when Tk_Sequence =>
                     Nb    := Get_Aggregate_Count (ACC);
                     El_TC := Unwind_Typedefs
                                (TypeCode.Content_Type (Data_Type));
                     --  Except for first element, which is an unsigned long

                  when others =>
                     --  Never happens
                     raise Program_Error;
               end case;

               pragma Assert (Nb = Get_Aggregate_Count (ACC));

               --  Avoid a check failure in the computation of the index loop
               --  below, in the case of a struct or exception without members.
               --  Nothing to marshall if Nb = 0.

               if Nb = 0 then
                  return;
               end if;

               --  Check whether to use fast path marshalling

               declare
                  use type System.Address;

                  Aggregate_Data      : System.Address;
                  Aggregate_Size      : Stream_Element_Count;
                  Aggregate_Alignment : Alignment_Type;
               begin
                  Fast_Path_Get_Info
                    (ACC                 => ACC'Access,
                     TC                  => Data_Type,
                     Buffer              => Buffer,
                     Aggregate_Data      => Aggregate_Data,
                     Aggregate_Size      => Aggregate_Size,
                     Aggregate_Alignment => Aggregate_Alignment);

                  if Aggregate_Data /= System.Null_Address then

                     --  Here we can do fast path marshalling, and we have the
                     --  underlying data address.

                     --  Special case for sequences: first marshall element
                     --  count. Note that there is an extra element in the
                     --  aggregate, which is the count itself, and is not part
                     --  of the fast path data.

                     if TCK = Tk_Sequence then
                        Marshall (Buffer, Nb - 1);
                     end if;

                     --  Now insert reference to data

                     Pad_Align (Buffer, Aggregate_Alignment);
                     Insert_Raw_Data
                       (Buffer,
                        Size => Aggregate_Size,
                        Data => Aggregate_Data);
                     return;
                  end if;

                  --  Fall through to per-element marshalling

               end;

               --  Here if marshalling aggregate element per element

               for J in 0 .. Nb - 1 loop

                  if J = 0 and then TCK = Tk_Sequence then

                     --  Special case of the first element of the sequence:
                     --  check consistency (it must be equal to the actual
                     --  element count).

                     declare
                        Count_C  : Any_Container;
                        Count_M  : aliased Mechanism := By_Value;
                        Count_CC : aliased Content'Class :=
                          Any.Get_Aggregate_Element
                            (ACC'Access,
                             TypeCode.PTC_Unsigned_Long'Access,
                             0, Count_M'Access);
                     begin
                        Set_Type (Count_C, TypeCode.TC_Unsigned_Long);
                        Set_Value (Count_C, Count_CC'Unchecked_Access);
                        if Nb - 1 /= From_Any (Count_C) then
                           raise Constraint_Error;
                        end if;
                     end;
                     Marshall_Aggregate_Element
                       (TypeCode.PTC_Unsigned_Long'Access, ACC'Access, J);

                  else
                     case TCK is
                        when Tk_Struct | Tk_Except =>
                           El_TC := TypeCode.Member_Type (Data_Type, J);

                        when others =>
                           null;
                     end case;
                     Marshall_Aggregate_Element (El_TC, ACC'Access, J);
                  end if;
               end loop;

            exception
               when others =>

                  --  Translate exception into a PolyORB runtime error.
                  --  We conservatively set the completion status to
                  --  Completed_Maybe, because at this point we do not have
                  --  enough information to do a better determination. However,
                  --  the caller may replace this value with a more specific
                  --  one when the error is caught (Completed_No when failure
                  --  is detected while marshalling a request, Completed_Yes
                  --  when it occurs while marshalling a No_Exception reply).
                  --  See similar discussion in Unmarshall_To_Any.

                  Throw
                    (Error,
                     Marshal_E,
                     System_Exception_Members'
                       (Minor     => 0,
                        Completed => Completed_Maybe));
                  --  XXX What is the proper minor here?
            end;

         when Tk_Alias =>
            --  Should never happen
            raise Program_Error;

         when Tk_Longlong =>
            Marshall (Buffer, PolyORB.Types.Long_Long'(From_Any (CData)));

         when Tk_Ulonglong =>
            Marshall (Buffer,
              PolyORB.Types.Unsigned_Long_Long'(From_Any (CData)));

         when Tk_Longdouble =>
            Marshall (Buffer, PolyORB.Types.Long_Double'(From_Any (CData)));

         when Tk_Widechar =>
            Marshall
              (CDR_Representation'Class (R.all),
               Buffer,
               PolyORB.Types.Wchar'(From_Any (CData)),
               Error);

         when Tk_Wstring =>

            --  We need to call the From_Any variant returning
            --  Standard.Wide_String here to account for both the bounded and
            --  unbounded cases.

            Marshall
              (CDR_Representation'Class (R.all),
               Buffer,
               To_PolyORB_Wide_String (From_Any (CData)),
               Error);

         when Tk_Value =>
            declare
               --  Aggregate_Nb, Member_Nb : PolyORB.Types.Unsigned_Long;
               --  Value_Modifier, Value_Type,
               --      Value_Visibility : PolyORB.Any.Any;
               --  Already_Marshalled : False_Seq := Empty_Seq;
            begin
               --  Marshall (Buffer, Default_Value_Tag);

               --  Aggregate_Nb := PolyORB.Any.Get_Aggregate_Count (Data);
               --  Member_Nb := (Aggregate_Nb - 3) / 3;
               --  I := 5;
               --  J := 0;
               --  while (J < Member_Nb) loop
               --     Member_Value := PolyORB.Any.Get_Aggregate_Element
               --       (Data,
               --    TypeCode.Member_Type (Data_Type, I + 3 * J),
               --        J);
               --     declare
               --        Member_Type : constant TypeCode.Local_Ref
               --          := PolyORB.Any.Get_Unwound_Type (Member_Value);
               --     begin
               --        case TypeCode.Kind (Member_Type) is
               --           when Tk_Value =>
               --              Marshall_From_Any
               --                (Buffer, Value, Already_Marshalled, 0);
               --           when others =>
               --              Marshall_From_Any (Buffer, Value);
               --        end case;
               --     end;
               --  end loop;
               raise Program_Error;
            end;

         when Tk_Valuebox =>
            declare
               ACC : Aggregate_Content'Class renames
                 Aggregate_Content'Class (Get_Value (CData).all);

            begin
               if Get_Aggregate_Count (ACC) = 0 then
                  Marshall (Buffer, Null_Tag);

               else
                  Marshall (Buffer, Value_Tag);

                  Marshall_Aggregate_Element
                    (TypeCode.Content_Type (Data_Type),
                     ACC'Access,
                     0);
               end if;
            end;

         when Tk_Native =>
            --  FIXME: TBD
            raise Program_Error;

         when Tk_Abstract_Interface =>
            --  FIXME: TBD
            raise Program_Error;

         when Tk_Local_Interface =>
            Throw
              (Error,
               Marshal_E,
               System_Exception_Members'
                 (Minor     => 4,
                  Completed => Completed_No));

         when Tk_Component =>
            --  FIXME : to be done
            raise Program_Error;

         when Tk_Home =>
            --  FIXME : to be done
            raise Program_Error;

         when Tk_Event =>
            --  FIXME : to be done
            raise Program_Error;
      end case;
      pragma Debug (C, O ("Marshall_From_Any: end"));
   end Marshall_From_Any;

   ----------------------
   -- Register_Factory --
   ----------------------

   procedure Register_Factory
     (Major   : Types.Octet;
      Minor   : Types.Octet;
      Factory : CDR_Representation_Factory)
   is
      use Factory_Lists;

      Iter : Iterator := First (Factory_Registry);
   begin
      while not Last (Iter) loop
         if Value (Iter).Major = Major
           and then Value (Iter).Minor = Minor
         then
            raise Program_Error;
         end if;

         Next (Iter);
      end loop;

      Append (Factory_Registry, (Major, Minor, Factory));
   end Register_Factory;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Representation : in out CDR_Representation) is
      use TC_Maps;
   begin
      Deallocate (Representation.TC_Map);
   end Release;

   --------------
   -- Start_TC --
   --------------

   procedure Start_TC
     (Representation : access CDR_Representation'Class;
      TC_Ref         : Any.TypeCode.Object_Ptr;
      Offset         : Types.Long;
      Complex        : Boolean)
   is
      use TC_Maps;
      T : TC_Maps.Instance renames Representation.TC_Map;
   begin
      pragma Debug (C, O ("Start_TC: Complex = " & Complex'Img
        & ", Current_Complex =" & Representation.Current_Complex'Img
        & ", (rel) Offset =" & Offset'Img));

      --  No mapping required for a simple typecode at the outermost level

      if Representation.Current_Complex = -1 then
         if not Complex then
            return;
         end if;

         Initialize (T);
      end if;

      Increment_Last (T);

      T.Table (Last (T)) :=
        (Enclosing_Complex => Representation.Current_Complex,
         TC_Ref            => TC_Ref,
         Offset            => To_Absolute_Offset (Representation, Offset));

      pragma Debug (C, O ("Start_TC: new entry @" & Last (T)'Img & ": "
                       & Image (T.Table (Last (T)))));

      if Complex then
         Representation.Current_Complex := Last (T);
      end if;
   end Start_TC;

   ------------------------
   -- To_Absolute_Offset --
   ------------------------

   function To_Absolute_Offset
     (Representation  : access CDR_Representation'Class;
      Relative_Offset : Types.Long) return Types.Long
   is
      T : TC_Maps.Instance renames Representation.TC_Map;
   begin
      if Representation.Current_Complex /= -1 then
         --  Offset is relative to the start of the encapsulation buffer of the
         --  of the immediately enclosing typecode, which itself is offset by
         --  8 bytes from its own absolute offset (typecode kind +
         --  encapsulation length).

         return Relative_Offset
           + T.Table (Representation.Current_Complex).Offset + 8;
      else
         return Relative_Offset;
      end if;
   end To_Absolute_Offset;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer         : access Buffer_Type;
      Representation : access CDR_Representation'Class) return PolyORB.Any.Any
   is
      Result : Any.Any;
      TC     : TypeCode.Local_Ref;
      E      : Errors.Error_Container;
   begin
      pragma Debug (C, O ("Unmarshall (Any): enter"));

      Unmarshall (Buffer, Representation, TC, E);
      pragma Assert (not Found (E));
      --  ??? Propagate error?

      Result := Get_Empty_Any (TC);
      Unmarshall_To_Any
        (Representation, Buffer, Get_Container (Result).all, E);
      pragma Assert (not Found (E));
      --  ??? Propagate error?

      pragma Debug (C, O ("Unmarshall (Any): end"));
      return Result;
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      R      : access CDR_Representation'Class;
      Data   : out TypeCode.Local_Ref;
      Error  : in out Errors.Error_Container)
   is
      Complex : Boolean := False;
      --  Set true in the case of a complex typecode that may contain nested
      --  typecodes (False for empty and simple typecodes, but also for complex
      --  typecodes that contain only elementary types).

      Complex_Buffer : aliased Buffer_Type;

      TypeCode_Id : constant Types.Unsigned_Long := Unmarshall (Buffer);
      Offset      : constant Types.Long :=
        Types.Long (CDR_Position (Buffer)) - (TypeCode_Id'Size / 8);
      --  Offset is the start position of TypeCode_Id in the buffer. Note that
      --  we cannot take the position before the call to Unmarshall, because
      --  we might need to skip some alignment padding first.

   begin
      pragma Debug (C, O ("Unmarshall (TypeCode): enter"));

      case TypeCode_Id is
         when TC_Null_Id =>
            Data := TypeCode.TC_Null;

         when TC_Void_Id =>
            Data := TypeCode.TC_Void;

         when TC_Short_Id =>
            Data := TypeCode.TC_Short;

         when TC_Long_Id =>
            Data := TypeCode.TC_Long;

         when TC_Unsigned_Short_Id =>
            Data := TypeCode.TC_Unsigned_Short;

         when TC_Unsigned_Long_Id =>
            Data := TypeCode.TC_Unsigned_Long;

         when TC_Float_Id =>
            Data := TypeCode.TC_Float;

         when TC_Double_Id =>
            Data := TypeCode.TC_Double;

         when TC_Boolean_Id =>
            Data := TypeCode.TC_Boolean;

         when TC_Char_Id =>
            Data := TypeCode.TC_Char;

         when TC_Octet_Id =>
            Data := TypeCode.TC_Octet;

         when TC_Any_Id =>
            Data := TypeCode.TC_Any;

         when TC_TypeCode_Id =>
            Data := TypeCode.TC_TypeCode;

         when TC_Principal_Id =>
            Data := TypeCode.TCF_Principal;

         when TC_Object_Id =>
            Data := TypeCode.TCF_Object;

            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Id, Name      : Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));

               TypeCode.Add_Parameter (Data, To_Any (Name));
               TypeCode.Add_Parameter (Data, To_Any (Id));
            end;

         when TC_Struct_Id =>
            Data := TypeCode.TCF_Struct;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Id, Name      : Types.String;

               Nb          : Types.Unsigned_Long;
               Member_Name : Types.String;
               Member_Type : TypeCode.Local_Ref;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));

               TypeCode.Add_Parameter (Data, To_Any (Name));
               TypeCode.Add_Parameter (Data, To_Any (Id));

               Complex := True;
               Start_TC
                 (R, Object_Of (Data), Offset, Complex => True);

               Nb := Unmarshall (Complex_Buffer'Access);
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name :=
                       Types.String
                         (Types.Identifier'
                            (Unmarshall (Complex_Buffer'Access)));

                     Unmarshall (Complex_Buffer'Access, R, Member_Type, Error);
                     exit when Found (Error);
                     Any.TypeCode.Add_Parameter (Data, To_Any (Member_Type));
                     Any.TypeCode.Add_Parameter (Data, To_Any (Member_Name));
                  end loop;
               end if;

               End_TC (R, Complex => True);
            end;

         when TC_Union_Id =>
            Data := TypeCode.TCF_Union;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);

               Id, Name, Member_Name : PolyORB.Types.String;
               Nb : PolyORB.Types.Unsigned_Long;

               --  Parameters for union TypeCode

               Default_Index : PolyORB.Types.Long;
               Discriminator_Type, Member_Type : TypeCode.Local_Ref;
               Member_Label : Any.Any;

               E : Error_Container;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));

               Any.TypeCode.Add_Parameter (Data, To_Any (Name));
               Any.TypeCode.Add_Parameter (Data, To_Any (Id));

               Complex := True;
               Start_TC
                 (R, Object_Of (Data), Offset, Complex => True);

               Unmarshall
                 (Complex_Buffer'Access, R, Discriminator_Type, Error);

               if Found (Error) then
                  --  Do not proceed further in error case

                  Nb := 0;

               else
                  Default_Index := Unmarshall (Complex_Buffer'Access);
                  Nb := Unmarshall (Complex_Buffer'Access);
                  TypeCode.Add_Parameter (Data, To_Any (Discriminator_Type));
                  TypeCode.Add_Parameter (Data, To_Any (Default_Index));

               end if;

               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Label := Get_Empty_Any (Discriminator_Type);
                     Unmarshall_To_Any
                       (R,
                        Complex_Buffer'Access,
                        Get_Container (Member_Label).all,
                        E);
                     pragma Assert (not Found (E));
                     Member_Name :=
                       Types.String
                         (Types.Identifier'
                            (Unmarshall (Complex_Buffer'Access)));
                     Unmarshall (Complex_Buffer'Access, R, Member_Type, Error);
                     exit when Found (Error);

                     TypeCode.Add_Parameter (Data, Member_Label);
                     TypeCode.Add_Parameter (Data, To_Any (Member_Type));
                     TypeCode.Add_Parameter (Data, To_Any (Member_Name));
                  end loop;
               end if;

               End_TC (R, Complex => True);
            end;

         when TC_Enum_Id =>
            Data := TypeCode.TCF_Enum;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Id, Name, Member_Name : PolyORB.Types.String;
               Nb : PolyORB.Types.Unsigned_Long;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));

               Any.TypeCode.Add_Parameter (Data, To_Any (Name));
               Any.TypeCode.Add_Parameter (Data, To_Any (Id));

               Complex := True;
               Start_TC
                 (R, Object_Of (Data), Offset, Complex => True);

               Nb := Unmarshall (Complex_Buffer'Access);

               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name :=
                       Types.String
                         (Types.Identifier'
                            (Unmarshall (Complex_Buffer'Access)));
                     TypeCode.Add_Parameter (Data, To_Any (Member_Name));
                  end loop;
               end if;

               End_TC (R, Complex => True);
            end;

         when TC_String_Id =>
            declare
               Length : constant PolyORB.Types.Unsigned_Long :=
                 Unmarshall (Buffer);
            begin
               Data := Build_Complex_TC
                 (Tk_String, (1 .. 1 => To_Any (Length)));
            end;

         when TC_Sequence_Id =>
            Data := TypeCode.TCF_Sequence;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Length : PolyORB.Types.Unsigned_Long;
               Content_Type : TypeCode.Local_Ref;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);

               Complex := True;
               Start_TC
                 (R, Object_Of (Data), Offset, Complex => True);

               Unmarshall (Complex_Buffer'Access, R, Content_Type, Error);

               if not Found (Error) then
                  Length := Unmarshall (Complex_Buffer'Access);
                  TypeCode.Add_Parameter (Data, To_Any (Length));
                  TypeCode.Add_Parameter (Data, To_Any (Content_Type));
               end if;

               End_TC (R, Complex => True);
            end;

         when TC_Array_Id =>
            Data := TypeCode.TCF_Array;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Length : PolyORB.Types.Unsigned_Long;
               Content_Type : TypeCode.Local_Ref;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);

               Complex := True;
               Start_TC (R, Object_Of (Data), Offset, Complex => True);

               Unmarshall (Complex_Buffer'Access, R, Content_Type, Error);

               if not Found (Error) then
                  Length := Unmarshall (Complex_Buffer'Access);
                  Any.TypeCode.Add_Parameter (Data, To_Any (Length));
                  Any.TypeCode.Add_Parameter (Data, To_Any (Content_Type));
               end if;

               End_TC (R, Complex => True);
            end;

         when TC_Alias_Id =>
            Data := TypeCode.TCF_Alias;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Id, Name : PolyORB.Types.String;
               Content_Type : TypeCode.Local_Ref;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));

               TypeCode.Add_Parameter (Data, To_Any (Name));
               TypeCode.Add_Parameter (Data, To_Any (Id));

               Complex := True;
               Start_TC (R, Object_Of (Data), Offset, Complex => True);

               Unmarshall (Complex_Buffer'Access, R, Content_Type, Error);
               if not Found (Error) then
                  Any.TypeCode.Add_Parameter (Data, To_Any (Content_Type));
               end if;

               End_TC (R, Complex => True);
            end;

         when TC_Except_Id =>
            Data := TypeCode.TCF_Except;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Id, Name, Member_Name : PolyORB.Types.String;
               Nb : PolyORB.Types.Unsigned_Long;
               Member_Type : TypeCode.Local_Ref;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));

               Any.TypeCode.Add_Parameter (Data, To_Any (Name));
               Any.TypeCode.Add_Parameter (Data, To_Any (Id));

               Complex := True;
               Start_TC
                 (R, Object_Of (Data), Offset, Complex => True);

               Nb := Unmarshall (Complex_Buffer'Access);

               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name :=
                       Types.String
                         (Types.Identifier'
                            (Unmarshall (Complex_Buffer'Access)));
                     Unmarshall (Complex_Buffer'Access, R, Member_Type, Error);
                     exit when Found (Error);

                     TypeCode.Add_Parameter (Data, To_Any (Member_Type));
                     TypeCode.Add_Parameter (Data, To_Any (Member_Name));
                  end loop;
               end if;

               End_TC (R, Complex => True);
            end;

         when TC_Long_Long_Id =>
            Data := TypeCode.TC_Long_Long;

         when TC_Unsigned_Long_Long_Id =>
            Data := TypeCode.TC_Unsigned_Long_Long;

         when TC_Long_Double_Id =>
            Data := TypeCode.TC_Long_Double;

         when TC_Wchar_Id =>
            Data := TypeCode.TC_Wchar;

         when TC_Wide_String_Id =>
            declare
               Length : constant PolyORB.Types.Unsigned_Long :=
                 Unmarshall (Buffer);
            begin
               Data := Build_Complex_TC
                         (Tk_Wstring, (1 .. 1 => To_Any (Length)));
            end;

         when TC_Fixed_Id =>
            Data := TypeCode.TCF_Fixed;
            declare
               Fixed_Digits : PolyORB.Types.Unsigned_Short;
               Fixed_Scale  : PolyORB.Types.Short;
            begin
               Fixed_Digits := Unmarshall (Buffer);
               Fixed_Scale  := Unmarshall (Buffer);
               TypeCode.Add_Parameter (Data, To_Any (Fixed_Digits));
               TypeCode.Add_Parameter (Data, To_Any (Fixed_Scale));
            end;

         when TC_Value_Id =>
            Data := TypeCode.TCF_Value;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Id, Name, Member_Name : PolyORB.Types.String;
               Type_Modifier, Visibility : PolyORB.Types.Short;
               Nb : PolyORB.Types.Unsigned_Long;
               Concrete_Base_Type, Member_Type : TypeCode.Local_Ref;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));

               Any.TypeCode.Add_Parameter (Data, To_Any (Name));
               Any.TypeCode.Add_Parameter (Data, To_Any (Id));

               Complex := True;
               Start_TC
                 (R, Object_Of (Data), Offset, Complex => True);

               Type_Modifier := Unmarshall (Complex_Buffer'Access);
               Unmarshall
                 (Complex_Buffer'Access, R, Concrete_Base_Type, Error);

               if Found (Error) then
                  --  Do not proceed further in error case

                  Nb := 0;

               else
                  Nb := Unmarshall (Complex_Buffer'Access);
               end if;

               TypeCode.Add_Parameter (Data, To_Any (Type_Modifier));
               TypeCode.Add_Parameter (Data, To_Any (Concrete_Base_Type));

               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name :=
                       Types.String
                         (Types.Identifier'
                            (Unmarshall (Complex_Buffer'Access)));
                     Unmarshall (Complex_Buffer'Access, R, Member_Type, Error);
                     exit when Found (Error);

                     Visibility := Unmarshall (Complex_Buffer'Access);
                     TypeCode.Add_Parameter (Data, To_Any (Visibility));
                     TypeCode.Add_Parameter (Data, To_Any (Member_Type));
                     TypeCode.Add_Parameter (Data, To_Any (Member_Name));
                  end loop;
               end if;

               End_TC (R, Complex => True);
            end;

         when TC_Valuebox_Id =>
            Data := TypeCode.TCF_Valuebox;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Id, Name : PolyORB.Types.String;
               Content_Type : TypeCode.Local_Ref;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));

               Any.TypeCode.Add_Parameter (Data, To_Any (Name));
               Any.TypeCode.Add_Parameter (Data, To_Any (Id));

               Complex := True;
               Start_TC
                 (R, Object_Of (Data), Offset, Complex => True);

               Unmarshall (Complex_Buffer'Access, R, Content_Type, Error);
               if not Found (Error) then
                  TypeCode.Add_Parameter (Data, To_Any (Content_Type));
               end if;

               End_TC (R, Complex => True);
            end;

         when TC_Native_Id =>
            Data := TypeCode.TCF_Native;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Id, Name : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               TypeCode.Add_Parameter (Data, To_Any (Name));
               TypeCode.Add_Parameter (Data, To_Any (Id));
            end;

         when TC_Abstract_Interface_Id =>
            Data := TypeCode.TCF_Abstract_Interface;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Id, Name : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               TypeCode.Add_Parameter (Data, To_Any (Name));
               TypeCode.Add_Parameter (Data, To_Any (Id));
            end;

         when TC_Local_Interface_Id =>
            Data := TypeCode.TCF_Local_Interface;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Id, Name : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               TypeCode.Add_Parameter (Data, To_Any (Name));
               TypeCode.Add_Parameter (Data, To_Any (Id));
            end;

         when TC_Component_Id =>
            Data := TypeCode.TCF_Component;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Id, Name : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               TypeCode.Add_Parameter (Data, To_Any (Name));
               TypeCode.Add_Parameter (Data, To_Any (Id));
            end;

         when TC_Home_Id =>
            Data := TypeCode.TCF_Home;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Id, Name      : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               TypeCode.Add_Parameter (Data, To_Any (Name));
               TypeCode.Add_Parameter (Data, To_Any (Id));
            end;

         when TC_Event_Id =>
            Data := TypeCode.TCF_Event;
            declare
               Complex_Encap : aliased Encapsulation := Unmarshall (Buffer);
               Id, Name, Member_Name : PolyORB.Types.String;
               Type_Modifier, Visibility : PolyORB.Types.Short;
               Nb : PolyORB.Types.Unsigned_Long;
               Concrete_Base_Type, Member_Type : TypeCode.Local_Ref;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               Type_Modifier := Unmarshall (Complex_Buffer'Access);
               Unmarshall
                 (Complex_Buffer'Access, R, Concrete_Base_Type, Error);

               if Found (Error) then
                  --  Do not attempt to proceed further in error case

                  Nb := 0;

               else
                  Nb := Unmarshall (Complex_Buffer'Access);

               end if;

               TypeCode.Add_Parameter (Data, To_Any (Name));
               TypeCode.Add_Parameter (Data, To_Any (Id));
               TypeCode.Add_Parameter (Data, To_Any (Type_Modifier));
               TypeCode.Add_Parameter (Data, To_Any (Concrete_Base_Type));

               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name :=
                       Types.String
                         (Types.Identifier'
                            (Unmarshall (Complex_Buffer'Access)));
                     Unmarshall (Complex_Buffer'Access, R, Member_Type, Error);
                     exit when Found (Error);
                     Visibility := Unmarshall (Complex_Buffer'Access);
                     TypeCode.Add_Parameter (Data, To_Any (Visibility));
                     TypeCode.Add_Parameter (Data, To_Any (Member_Type));
                     TypeCode.Add_Parameter (Data, To_Any (Member_Name));
                  end loop;
               end if;
            end;

         when TC_Indirect =>
            declare
               Current : constant Types.Long :=
                 Types.Long (CDR_Position (Buffer));
               Offset  : constant Types.Long := Unmarshall (Buffer);
            begin
               if Offset >= -4 then
                  raise Constraint_Error;
               end if;

               pragma Debug (C, O ("Unmarshall (TypeCode): @" & Current'Img
                 & ": found indirect reference with relative offset "
                 & Offset'Img));

               Data := To_Ref
                 (Find_TC (R,
                    Offset => To_Absolute_Offset (R, Current) + Offset));
            end;

         when others =>
            raise Constraint_Error;
      end case;

      --  For an empty of simple typecode, record in map now

      if not Complex then
         Start_TC (R, Object_Of (Data), Offset, Complex => False);
         End_TC   (R, Complex => False);
      end if;

      pragma Debug (C, O ("Unmarshall (TypeCode): end"));
   end Unmarshall;

   -----------------------
   -- Unmarshall_To_Any --
   -----------------------

   overriding procedure Unmarshall_To_Any
     (R      : access CDR_Representation;
      Buffer : not null access Buffer_Type;
      CData  : in out Any.Any_Container'Class;
      Error  : in out Errors.Error_Container)
   is
      TC  : constant TypeCode.Object_Ptr :=
        Unwind_Typedefs (Get_Type_Obj (CData));
      TCK : constant TCKind := TypeCode.Kind (TC);
   begin
      pragma Debug
        (C, O ("Unmarshall_To_Any: Any_Type is " &
            PolyORB.Any.TCKind'Image (TypeCode.Kind (TC))));

      case TCK is

         when Tk_Null | Tk_Void =>
            null;

         when Tk_Short =>
            declare
               S : constant Short := Unmarshall (Buffer);
            begin
               pragma Debug (C, O ("Unmarshall_To_Any: value is "
                                & PolyORB.Types.Short'Image (S)));
               Set_Any_Value (S, CData);
            end;

         when Tk_Long =>
            declare
               L : constant Long := Unmarshall (Buffer);
            begin
               Set_Any_Value (L, CData);
            end;

         when Tk_Ushort =>
            declare
               Us : constant Unsigned_Short := Unmarshall (Buffer);
            begin
               Set_Any_Value (Us, CData);
            end;

         when Tk_Ulong =>
            declare
               Ul : constant Unsigned_Long := Unmarshall (Buffer);
            begin
               Set_Any_Value (Ul, CData);
            end;

         when Tk_Float =>
            declare
               F : constant PolyORB.Types.Float := Unmarshall (Buffer);
            begin
               Set_Any_Value (F, CData);
            end;

         when Tk_Double =>
            declare
               D : constant Double := Unmarshall (Buffer);
            begin
               pragma Debug
                 (C, O ("Unmarshall_To_Any: value is " & Double'Image (D)));
               Set_Any_Value (D, CData);
            end;

         when Tk_Boolean =>
            declare
               B : constant PolyORB.Types.Boolean := Unmarshall (Buffer);
            begin
               Set_Any_Value (B, CData);
            end;

         when Tk_Char =>
            declare
               C : Char;
            begin
               Unmarshall (CDR_Representation'Class (R.all), Buffer, C, Error);
               Set_Any_Value (C, CData);
            end;

         when Tk_Octet =>
            declare
               O : constant PolyORB.Types.Octet := Unmarshall (Buffer);
            begin
               Set_Any_Value (O, CData);
            end;

         when Tk_Any =>
            declare
               A : constant Any.Any :=
                 Unmarshall (Buffer, CDR_Representation'Class (R.all)'Access);
            begin
               Set_Any_Value (A, CData);
            end;

         when Tk_TypeCode =>
            declare
               TC : TypeCode.Local_Ref;
            begin
               Unmarshall (Buffer, R, TC, Error);
               if not Found (Error) then
                  Set_Any_Value (TC, CData);
               end if;
            end;

         when Tk_Principal =>
            --  FIXME : to be done
            raise Program_Error;

         when Tk_Objref =>
            PolyORB.Any.ObjRef.Set_Any_Value (Unmarshall (Buffer), CData);

         when
           Tk_Struct   |
           Tk_Except   |
           Tk_Enum     |
           Tk_Union    |
           Tk_Sequence |
           Tk_Array    |
           Tk_Fixed    =>

            --  Common code for aggregates
            --  See comments in PolyORB.Any spec for detailed structure of
            --  aggregate for each TCKind.

            declare
               Nb    : Unsigned_Long;
               First_Index : Unsigned_Long;
               El_TC : TypeCode.Object_Ptr;

            begin

               --  For most aggregates, elements are stored at indices starting
               --  at 0, with the exception of sequences, where index 0 holds
               --  a copy of the sequence length, and elements are stored
               --  starting at index 1.

               First_Index := 0;

               case TCK is
                  when Tk_Struct | Tk_Except =>
                     Nb := TypeCode.Member_Count (TC);

                  when Tk_Enum =>
                     Nb := 1;
                     El_TC := TypeCode.PTC_Unsigned_Long'Access;

                  when Tk_Union =>
                     Nb := 2;
                     --  Note: reset to 1 if there is no element associated
                     --  with the unmarshalled switch value.

                  when Tk_Sequence =>
                     declare
                        Max_Length : constant Types.Unsigned_Long :=
                          TypeCode.Length (TC);
                     begin
                        Nb := Unmarshall (Buffer);
                        if Max_Length > 0 and then Nb > Max_Length then
                           raise Constraint_Error;
                        end if;
                     end;
                     Nb := Nb + 1;
                     First_Index := 1;
                     El_TC := TypeCode.Content_Type (TC);

                  when Tk_Array =>
                     Nb    := TypeCode.Length (TC);
                     El_TC := TypeCode.Content_Type (TC);

                  when Tk_Fixed =>
                     Nb    := (Types.Unsigned_Long
                               (TypeCode.Fixed_Digits (TC)) + 2) / 2;
                     El_TC := TypeCode.PTC_Octet'Access;

                  when others =>
                     --  Never happens
                     raise Program_Error;
               end case;

               if Is_Empty (CData) then
                  Set_Any_Aggregate_Value (CData);
               end if;

               declare
                  use type System.Address;

                  ACC : Aggregate_Content'Class renames
                    Aggregate_Content'Class (Get_Value (CData).all);

                  Val_TC : TypeCode.Object_Ptr;
                  --  Value typecode, computed from label TC in case of a union

                  Aggregate_Data      : System.Address;
                  Aggregate_Size      : Stream_Element_Count;
                  Aggregate_Alignment : Alignment_Type;
                  --  Information used for fast path unmarshalling

               begin
                  Set_Aggregate_Count (ACC, Nb);

                  --  Now that we have an aggregate of the correct size (i.e.
                  --  in particular we have allocated space to store the
                  --  unmarshalled data), check whether to use fast path
                  --  unmarshalling.

                  Fast_Path_Get_Info
                    (ACC                 => ACC'Access,
                     TC                  => TC,
                     Buffer              => Buffer,
                     Aggregate_Data      => Aggregate_Data,
                     Aggregate_Size      => Aggregate_Size,
                     Aggregate_Alignment => Aggregate_Alignment);

                  if Aggregate_Data /= System.Null_Address then

                     --  Here we can do fast path unmarshalling, and we have
                     --  the underlying data address. Note that in the case of
                     --  fast path unmarshalling, data may be fragmented in the
                     --  buffer, so do reassembly now.

                     declare
                        Data : Stream_Element_Array (1 .. Aggregate_Size);
                        for Data'Address use Aggregate_Data;
                        pragma Import (Ada, Data);
                     begin
                        Utils.Buffers.Align_Unmarshall_Copy
                          (Buffer,
                           Aggregate_Alignment,
                           Data);
                     end;
                     return;
                  end if;

                  --  Fall through to per-element unmarshalling

                  if TCK = Tk_Sequence then
                     declare
                        Len_M  : aliased Mechanism := By_Reference;
                        Len_CC : aliased Content'Class :=
                          Get_Aggregate_Element
                            (ACC'Access,
                             TypeCode.PTC_Unsigned_Long'Access,
                             0, Len_M'Access);
                        Len_C : Any_Container;
                     begin
                        Set_Type (Len_C, TypeCode.TC_Unsigned_Long);
                        if Len_CC not in No_Content then
                           Set_Value (Len_C, Len_CC'Unchecked_Access);
                        end if;
                        Set_Any_Value (Nb - 1, Len_C);
                        if Len_M = By_Value then
                           Set_Aggregate_Element
                             (ACC, TypeCode.PTC_Unsigned_Long'Access, 0,
                              From_C => Len_C);
                           Finalize_Value (Len_C);
                        end if;
                     end;

                  end if;

                  --  If there are no elements to get, return here.
                  --  Note: Nb is a Types.Unsigned_Long, which is a modular
                  --  integer type, so we must be careful to not underflow it
                  --  by writing "Nb - 1" for the case of the zero value.

                  if First_Index + 1 > Nb then
                     return;
                  end if;

                  Unmarshall_Elements :
                  for J in First_Index .. Nb - 1 loop
                     pragma Debug (C, O ("Unmarshall_To_Any: get element"));

                     --  Determine aggregate element typecode

                     case TCK is
                        when Tk_Struct | Tk_Except =>
                           El_TC := TypeCode.Member_Type (TC, J);

                        when Tk_Union =>
                           if J = 0 then
                              El_TC := TypeCode.Discriminator_Type (TC);
                           else
                              El_TC := Val_TC;
                           end if;

                        when Tk_Sequence | Tk_Array | Tk_Fixed | Tk_Enum =>

                           --  El_TC has been set once and for all before
                           --  entering the elements loop

                           null;

                        when others =>
                           --  Never happens

                           raise Program_Error;
                     end case;

                     --  Unmarshall element into shadow container

                     declare
                        function El_Info return String;
                        --  Description of member, for debugging

                        -------------
                        -- El_Info --
                        -------------

                        function El_Info return String is
                           El_Name : Types.Identifier;
                        begin
                           case TCK is
                              when Tk_Struct | Tk_Except =>
                                 El_Name := TypeCode.Member_Name (TC, J);

                              when Tk_Union =>
                                 if J = 0 then
                                    El_Name := To_PolyORB_String ("<switch>");
                                 else
                                    El_Name := To_PolyORB_String ("<value>");
                                 end if;

                              when
                                Tk_Sequence | Tk_Array | Tk_Fixed | Tk_Enum =>
                                 El_Name := To_PolyORB_String
                                              ("<element" & J'Img & ">");

                              when others =>
                                 --  Never happens

                                 raise Program_Error;
                           end case;
                           return TCK'Img & " member " & To_String (El_Name);
                        end El_Info;

                        El_C  : Any_Container;
                        El_M  : aliased Mechanism := By_Reference;
                        El_CC : aliased Content'Class :=
                          Get_Aggregate_Element (ACC'Access, El_TC, J,
                                                 El_M'Access);
                     begin
                        Set_Type (El_C, El_TC);
                        if El_CC not in No_Content then
                           Set_Value (El_C, El_CC'Unchecked_Access);
                        end if;

                        pragma Debug (C, O ("Unmarshall_To_Any: about to "
                          & "unmarshall " & El_Info));

                        Unmarshall_To_Any
                          (CDR_Representation'Class (R.all)'Access,
                           Buffer,
                           El_C,
                           Error);

                        if Found (Error) then
                           if El_M = By_Value then
                              Finalize_Value (El_C);
                           end if;
                           return;
                        end if;

                        if TCK = Tk_Union and then J = 0 then
                           Val_TC :=
                             TypeCode.Member_Type_With_Label (TC, El_C);
                        end if;

                        if El_M = By_Value then
                           pragma Debug (C, O ("Setting element By_Value"));
                           Set_Aggregate_Element
                             (ACC, El_TC, J, From_C => El_C);
                           Finalize_Value (El_C);
                        end if;

                        --  Handle the case of a union with no member for
                        --  this label: nothing to do once the switch element
                        --  has been set.

                        if TCK = Tk_Union
                          and then J = 0
                          and then Any.TypeCode.Kind (Val_TC) = Tk_Void
                        then
                           Set_Aggregate_Count (ACC, 1);
                           exit Unmarshall_Elements;
                        end if;
                     end;
                  end loop Unmarshall_Elements;
               end;
            exception
               when others =>

                  --  Translate exception into a PolyORB runtime error.
                  --  We conservatively set the completion status to
                  --  Completed_Maybe, because at this point we do not have
                  --  enough information to do a better determination.
                  --  However, the caller may replace this value with a more
                  --  specific one when the error is caught (Completed_No
                  --  when failure is detected while unmarshalling a request,
                  --  Completed_Yes when it occurs while unmarshalling a
                  --  No_Exception reply). See similar discussion in
                  --  Marshall_From_Any.

                  Throw
                    (Error,
                     Marshal_E,
                     System_Exception_Members'
                       (Minor     => 0,
                        Completed => Completed_Maybe));
                  --  XXX What is the proper minor here?
            end;

         when Tk_String =>
            declare
               S : PolyORB.Types.String;
            begin
               Unmarshall
                 (CDR_Representation'Class (R.all),
                  Buffer,
                  S,
                  Error);

               declare
                  Bound : constant Types.Unsigned_Long :=
                    TypeCode.Length (TC);
               begin
                  if Bound = 0 then
                     Set_Any_Value (S, CData);
                  else
                     Set_Any_Value (Types.To_Standard_String (S),
                                    Positive (Bound), CData);
                  end if;
               end;
            end;

         when Tk_Alias =>
            --  We should never reach this point
            raise Program_Error;

         when Tk_Longlong =>
            declare
               Ll : constant Long_Long := Unmarshall (Buffer);
            begin
               Set_Any_Value (Ll, CData);
            end;

         when Tk_Ulonglong =>
            declare
               Ull : constant Unsigned_Long_Long := Unmarshall (Buffer);
            begin
               Set_Any_Value (Ull, CData);
            end;

         when Tk_Longdouble =>
            declare
               Ld : constant Long_Double := Unmarshall (Buffer);
            begin
               Set_Any_Value (Ld, CData);
            end;

         when Tk_Widechar =>
            declare
               Wc : Wchar;
            begin
               Unmarshall
                 (CDR_Representation'Class (R.all),
                  Buffer,
                  Wc,
                  Error);
               Set_Any_Value (Wc, CData);
            end;

         when Tk_Wstring =>
            declare
               Ws : PolyORB.Types.Wide_String;
            begin
               Unmarshall
                 (CDR_Representation'Class (R.all),
                  Buffer,
                  Ws,
                  Error);

               declare
                  Bound : constant Types.Unsigned_Long :=
                    TypeCode.Length (TC);
               begin
                  if Bound = 0 then
                     Set_Any_Value (Ws, CData);
                  else
                     Set_Any_Value (Types.To_Wide_String (Ws),
                                    Positive (Bound), CData);
                  end if;
               end;
            end;

         when Tk_Value =>

            --  declare
            --   Val_Modifier,Arg: PolyORB.Any.Any;
            --   Nb: PolyORB.Types.Unsigned_Long:=
            --          TypeCode.Member_Count(TC);

            --  begin
            --   Set_Any_Aggregate_Value(Result);
            --   if Is_Empty then
            --     Val_Modifier:= Get_Empty_Any(TypeCode.Type_Modifier(TC));
            --   else
            --     Val_Modifier:= Get_Aggregate_Element
            --               (Result,
            --                TypeCode.Discriminator_Type(TC),
            --                PolyORB.Types.Unsigned_Long(0));
            --   end if;
            --   Unmarshall_To_Any(Buffer,Val_Modifier);
            --   if Is_Empty then
            --     Add_Aggregate_Element(Result,Val_Modifier);
            --   end if;

            --   if Nb /=0 then
            --    for I in 0 .. Nb-1 loop
            --     if Is_Empty then
            --        Arg:= Get_Empty_Any( TypeCode.Member_Visibility(TC));
            --     else
            --        Arg:= Get_Aggregate_Element
            --               (Result,
            --                TypeCode.Member_Visibility(TC,I+1),
            --                I+1);
            --     end if;
            --     Unmarshall_To_Any(Buffer,Arg);
            --     if Is_Empty  then
            --       Add_Aggregate_Element(Result,Arg);
            --     end if;
            --    end loop;
            --   end if;
            --   end;
            raise Program_Error;

         when Tk_Valuebox =>
            declare
               Tag : constant PolyORB.Types.Unsigned_Long :=
                 Unmarshall (Buffer);

            begin
               if Is_Empty (CData) then
                  Set_Any_Aggregate_Value (CData);
               end if;

               declare
                  ACC : Aggregate_Content'Class renames
                    Aggregate_Content'Class (Get_Value (CData).all);

               begin
                  if Tag = Null_Tag then
                     Set_Aggregate_Count (ACC, 0);

                  else
                     Set_Aggregate_Count (ACC, 1);

                     declare
                        El_C  : Any_Container;
                        El_M  : aliased Mechanism := By_Reference;
                        El_CC : aliased Content'Class :=
                          Get_Aggregate_Element
                            (ACC'Access,
                             TypeCode.Content_Type (TC),
                             0,
                             El_M'Access);

                     begin
                        Set_Type (El_C, TypeCode.Content_Type (TC));

                        if El_CC not in No_Content then
                           Set_Value (El_C, El_CC'Unchecked_Access);
                        end if;

                        Unmarshall_To_Any
                          (CDR_Representation'Class (R.all)'Access,
                           Buffer,
                           El_C,
                           Error);

                        if El_M = By_Value then
                           Set_Aggregate_Element
                             (ACC,
                              TypeCode.Content_Type (TC),
                              0,
                              From_C => El_C);
                           Finalize_Value (El_C);
                        end if;
                     end;
                  end if;
               end;
            end;

         when Tk_Native =>
            --  FIXME : to be done
            raise Program_Error;

         when Tk_Abstract_Interface =>
            --  FIXME : to be done
            raise Program_Error;

         when Tk_Local_Interface =>
            Throw
              (Error,
               Marshal_E,
               System_Exception_Members'
                 (Minor     => 4,
                  Completed => Completed_No));

         when Tk_Component =>
            raise Program_Error;

         when Tk_Home =>
            raise Program_Error;

         when Tk_Event =>
            raise Program_Error;
      end case;
      pragma Debug (C, O ("Unmarshall_To_Any: end"));
   end Unmarshall_To_Any;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"representations.cdr",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Representations.CDR;
