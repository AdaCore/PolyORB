------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E P R E S E N T A T I O N S . D N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------
   with Ada.Streams;
   with System.Address_Image;
   with System;
   with PolyORB.Initialization;
   with PolyORB.Log;
   with PolyORB.Parameters;
   with GNAT.Byte_Swapping;
   with PolyORB.Utils.Strings;
   with PolyORB.Errors;
   with PolyORB.Utils.Buffers;
   pragma Elaborate_All (PolyORB.Utils.Buffers);

package body PolyORB.Representations.DNS is
   use Ada.Streams;
   use PolyORB.Any;

   use PolyORB.Any.TypeCode;
   use PolyORB.Log;
   use PolyORB.Errors;
   use PolyORB.Utils.Buffers;
   use PolyORB.Parameters;
   use GNAT.Byte_Swapping;

   package L is new PolyORB.Log.Facility_Log ("polyorb.representations.dns");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   Enable_Fast_Path : Boolean;
   --  If True, some aggregates are allowed to be marshalled in one lump of
   --  data (instead of element per element), if they have a suitable memory
   --  representation.

   procedure Initialize;
   procedure Initialize is
   begin
      Enable_Fast_Path :=
        Get_Conf ("dns", "enable_fast_path", Default => True);
   end Initialize;
   function Fast_Path_Element_Size
     (El_TCK : TCKind) return Types.Unsigned_Long;
   --  For a type that is a suitable element type for fast path marshalling
   --  of a sequence or array, return the type size. Otherwise return 0.

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

   procedure Fast_Path_Get_Info
     (ACC                 : access Aggregate_Content'Class;
      TC                  : TypeCode.Object_Ptr;
      Buffer              : access Buffer_Type;
      Aggregate_Data      : out System.Address;
      Aggregate_Size      : out Stream_Element_Count;
      Aggregate_Alignment : out Alignment_Type);

   procedure Fast_Path_Get_Info
     (ACC                 : access Aggregate_Content'Class;
      TC                  : TypeCode.Object_Ptr;
      Buffer              : access Buffer_Type;
      Aggregate_Data      : out System.Address;
      Aggregate_Size      : out Stream_Element_Count;
      Aggregate_Alignment : out Alignment_Type)
   is
      TCK : constant TCKind := TypeCode.Kind (TC);
   begin
      Aggregate_Data := System.Null_Address;

      if not Enable_Fast_Path then
         pragma Warnings (Off);
         --  OUT parameters Aggregate_Size and Aggregate_Element not set as
         --  they are meaningless when Aggregate_Data is null.
         return;
         pragma Warnings (On);
      end if;

      if TCK = Tk_Array or else TCK = Tk_Sequence then
         declare
            El_TC    : constant TypeCode.Object_Ptr :=
                         Unwind_Typedefs (TypeCode.Content_Type (TC));

            El_Size  : constant Types.Unsigned_Long :=
                         Fast_Path_Element_Size (TypeCode.Kind (El_TC));

            El_Count : Types.Unsigned_Long;
         begin
            if El_Size = 0 then
               --  Case of element type that does not allow fast path

               return;

            elsif El_Size > 1 and then Endianness (Buffer) /= Host_Order then
               --  Case of multi-byte elements, where the expected buffer
               --  endianness is not the host endianness: need to perform
               --  per-element byte swapping.

               return;
            end if;

            if TCK = Tk_Array then
               El_Count := TypeCode.Length (TC);
            else
               --  Aggregate elements count for a sequence has one additional
               --  element corresponding to the sequence length, which is not
               --  part of the fast path data.

               El_Count := Get_Aggregate_Count (ACC.all) - 1;
            end if;

            Aggregate_Data      := Unchecked_Get_V (ACC);
            Aggregate_Alignment :=
              Alignment_Of (Short_Short_Integer (El_Size));
            Aggregate_Size      := Stream_Element_Count (El_Count * El_Size);

            pragma Debug (C, O ("Fast_Path_Get_Info:"
              & Aggregate_Size'Img & " bytes ("
              & El_Count'Img & " elements) at "
              & System.Address_Image (Aggregate_Data)
              & ", align on" & Aggregate_Alignment'Img));
         end;
      end if;
   end Fast_Path_Get_Info;

   -----------------------
   -- Marshall_From_Any --
   -----------------------
   procedure Marshall_From_Any
     (Buffer : access Buffers.Buffer_Type;
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

         pragma Debug (C, O ("Marshall_From_Any: in Marsh_Agg_El:"
           & El_C.Image));

         --   XXX: this fix is temporary..
         if El_C.Image = "PTR" then
            Marshall (Buffer, Types.Unsigned_Short (12));
            Marshall (Buffer, Types.Unsigned_Short (1));
         else
            Marshall_From_Any (Buffer, El_C, Error);
         end if;
      end Marshall_Aggregate_Element;

      TCK : constant TCKind := TypeCode.Kind (Data_Type);

   begin
      pragma Debug (C, O ("Marshall_From_Any: enter"));
      pragma Debug (C, O ("Marshall_From_Any: CData = " & Image (CData)));

      case TCK is

         when Tk_Null | Tk_Void =>
            null;

         when Tk_Ulong =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(From_Any (CData)));

         when Tk_Boolean =>
            Marshall (Buffer, PolyORB.Types.Boolean'(From_Any (CData)));

         when Tk_Octet =>
            Marshall (Buffer, PolyORB.Types.Octet'(From_Any (CData)));

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
               Marshall_From_Any (Buffer, Label_C, Error);
               if Found (Error) then
                  return;
               end if;

               pragma Debug (C, O ("Marshall_From_Any: "
                 & "union label marshalled"));

               declare
                  Member_TC : constant Any.TypeCode.Object_Ptr :=
                                Any.TypeCode.Member_Type_With_Label
                                  (Data_Type, Label_C);
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

            Marshall_Latin_1_String
              (Buffer,
               To_PolyORB_String (From_Any (CData)));

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
                     pragma Debug (C, O ("Marshall_From_Any:Tk_Sequence:" &
                      Nb'Img & " elements in sequence "));
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
                        Marshall (Buffer, Types.Unsigned_Short (Nb - 1));
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
                     --  First element of a sequence is Unsigned_Long.
                     --  No need to marshalkl it in the DNS context
                     null;
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

                  Throw
                    (Error,
                     Marshal_E,
                     System_Exception_Members'
                       (Minor     => 0,
                        Completed => Completed_Maybe));
            end;
         when others =>
            raise Program_Error;
      end case;
      pragma Debug (C, O ("Marshall_From_Any: end"));
   end Marshall_From_Any;

   -----------------------
   -- Unmarshall_To_Any --
   -----------------------
   procedure Unmarshall_To_Any
     (Buffer : access Buffer_Type;
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

         when Tk_Boolean =>
            declare
               B : constant PolyORB.Types.Boolean := Unmarshall (Buffer);
            begin
               Set_Any_Value (B, CData);
            end;

         when Tk_Octet =>
            declare
               O : constant PolyORB.Types.Octet := Unmarshall (Buffer);
            begin
               Set_Any_Value (O, CData);
            end;

--           when Tk_Any =>
--              declare
--                 A : constant Any.Any :=
--                Unmarshall (Buffer, DNS_Representation'Class (R.all)'Access);
--              begin
--                 Set_Any_Value (A, CData);
--              end;

--           when Tk_TypeCode =>
--              declare
--                 TC : TypeCode.Local_Ref;
--              begin
--                 Unmarshall (Buffer, R, TC, Error);
--                 if not Found (Error) then
--                    Set_Any_Value (TC, CData);
--                 end if;
--              end;

--           when Tk_Objref =>
--              PolyORB.Any.ObjRef.Set_Any_Value (Unmarshall (Buffer), CData);

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
                          & "unmarshall a member"));

                        Unmarshall_To_Any
                          (Buffer,
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
               S := Types.To_PolyORB_String (Unmarshall_Latin_1_String
                  (Buffer));

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

         when Tk_Native =>
            --  FIXME : to be done
            raise Program_Error;

         when Tk_Abstract_Interface =>
            --  FIXME : to be done
            raise Program_Error;

         when Tk_Component =>
            raise Program_Error;

         when Tk_Home =>
            raise Program_Error;

         when Tk_Event =>
            raise Program_Error;
         when others =>
            raise Program_Error;
      end case;
      pragma Debug (C, O ("Unmarshall_To_Any: end"));
   end Unmarshall_To_Any;

      --  Marshalling of a Boolean

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Boolean)
   is
   begin
      pragma Debug (C, O ("Marshall (Boolean) : enter"));
      Marshall
        (Buffer, PolyORB.Types.Octet'(PolyORB.Types.Boolean'Pos (Data)));
      pragma Debug (C, O ("Marshall (Boolean) : end"));
   end Marshall;

   --  Marshalling of a Character

   procedure Marshall_Latin_1_Char
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Char)
   is
   begin
      pragma Debug (C, O ("Marshall (Char) : enter"));
      Marshall (Buffer, PolyORB.Types.Octet'(PolyORB.Types.Char'Pos (Data)));
      pragma Debug (C, O ("Marshall (Char) : end"));
   end Marshall_Latin_1_Char;

   function Unmarshall_Latin_1_Char
     (Buffer : access Buffer_Type) return PolyORB.Types.Char
   is
   begin
      pragma Debug (C, O ("Unmarshall (Char) : enter & end"));
      return PolyORB.Types.Char'Val
        (PolyORB.Types.Octet'(Unmarshall (Buffer)));
   end Unmarshall_Latin_1_Char;
   --  Marshalling of an Octet

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Octet)
   is
   begin
      pragma Debug (C, O ("Marshall (Octet) : enter"));
      Align_Marshall_Copy (Buffer, (1 => Stream_Element
                           (PolyORB.Types.Octet'(Data))), Align_1);
      pragma Debug (C, O ("Marshall (Octet) : end"));
   end Marshall;

      --  Marshalling of an Identifier
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Identifier)
   is
   begin
      pragma Debug (C, O ("Marshall (Identifier) : enter"));
      Marshall_Latin_1_String (Buffer, PolyORB.Types.String (Data));
      pragma Debug (C, O ("Marshall (Identifier) : end"));
   end Marshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Identifier
   is
   begin
      pragma Debug (C, O ("Unmarshall (Identifier) : enter & end"));
      return PolyORB.Types.Identifier
        (PolyORB.Types.String'(Types.To_PolyORB_String
         (Unmarshall_Latin_1_String (Buffer))));
   end Unmarshall;

   procedure Marshall_Latin_1_String
     (Buffer : access Buffer_Type;
      Data   : Standard.String)
   is
      Str : Stream_Element_Array (1 .. Data'Length);
      for Str'Address use Data'Address;
      pragma Import (Ada, Str);

   begin
      pragma Debug (C, O ("Marshall (String) : enter"));

      Marshall (Buffer, PolyORB.Types.Octet'(Data'Length));
      Align_Marshall_Copy (Buffer, Str);
      Marshall (Buffer, PolyORB.Types.Octet'(0));

      pragma Debug (C, O ("Marshall (String) : end"));
   end Marshall_Latin_1_String;

   --  Marshalling of a PolyORB.Types.String

   procedure Marshall_Latin_1_String
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.String)
   is
   begin
      pragma Debug (C, O ("Marshall (PolyORB.Types.String) : enter"));
      Marshall_Latin_1_String
        (Buffer, PolyORB.Types.To_Standard_String (Data));
      pragma Debug (C, O ("Marshall (PolyORB.Types.String) : end"));
   end Marshall_Latin_1_String;

   function Unmarshall_DNS_String
     (Buffer : access Buffer_Type;
      Length : Types.Unsigned_Short)
      return Standard.String
   is
      Equiv  : String (1 .. Natural (Length));
      End_Oct : Types.Octet;
   begin
      pragma Debug (C, O ("Unmarshall (String): enter"));
      pragma Debug (C, O ("Unmarshall (String): length is " &
                    PolyORB.Types.Unsigned_Short'Image (Length)));

      if Length = 0 then
         return "";
      end if;

      for J in Equiv'Range loop
         Equiv (J) := Character'Val
           (PolyORB.Types.Char'Pos (Unmarshall_Latin_1_Char (Buffer)));
      end loop;
      End_Oct := Unmarshall (Buffer);
      if End_Oct /= Types.Octet (0) then
         raise Constraint_Error;
      end if;

      pragma Debug (C, O ("Unmarshall (String): -> " & Equiv));
      return Equiv;
   end Unmarshall_DNS_String;

   function Unmarshall_Latin_1_String
     (Buffer : access Buffer_Type)
     return Standard.String
   is
      Length : constant PolyORB.Types.Unsigned_Short
        := Unmarshall (Buffer);
      Equiv  : String (1 .. Natural (Length) - 1);

   begin
      pragma Debug (C, O ("Unmarshall (String): enter"));
      pragma Debug (C, O ("Unmarshall (String): length is " &
                    PolyORB.Types.Unsigned_Short'Image (Length)));

      if Length = 0 then
         return "";
      end if;

      for J in Equiv'Range loop
         Equiv (J) := Character'Val
           (PolyORB.Types.Char'Pos (Unmarshall_Latin_1_Char (Buffer)));
      end loop;

      if Character'Val
           (PolyORB.Types.Char'Pos (Unmarshall_Latin_1_Char (Buffer)))
        /= ASCII.NUL
      then
         raise Constraint_Error;
      end if;

      pragma Debug (C, O ("Unmarshall (String): -> " & Equiv));

      return Equiv;
   end Unmarshall_Latin_1_String;

   ------------------------------------
   -- Unmarshall-by-copy subprograms --
   ------------------------------------

   function Unmarshall
     (Buffer : access Buffer_Type) return PolyORB.Types.Boolean
   is
   begin
      pragma Debug (C, O ("Unmarshall (Boolean) : enter & end"));
      return PolyORB.Types.Boolean'Val
        (PolyORB.Types.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   function Swapped (X : Types.Octet) return Types.Octet;
   pragma Inline (Swapped);
   package DNS_Octet is
     new Align_Transfer_Elementary (T => PolyORB.Types.Octet);
   function Unmarshall
     (Buffer : access Buffer_Type) return PolyORB.Types.Octet
      renames DNS_Octet.Unmarshall;
   function Swapped (X : Types.Octet) return Types.Octet is
   begin
      return X;
   end Swapped;

   function Swapped (X : Types.Unsigned_Long) return Types.Unsigned_Long;
   pragma Inline (Swapped);
   package DNS_Unsigned_Long is
     new Align_Transfer_Elementary (T => PolyORB.Types.Unsigned_Long);
   function Unmarshall
     (Buffer : access Buffer_Type) return PolyORB.Types.Unsigned_Long
      renames DNS_Unsigned_Long.Unmarshall;
   function Swapped (X : Types.Unsigned_Long) return Types.Unsigned_Long is
   begin
      return X;
   end Swapped;
   procedure Marshall
     (Buffer : access Buffer_Type; Data : PolyORB.Types.Unsigned_Long)
      renames DNS_Unsigned_Long.Marshall;

   function Swapped is
     new GNAT.Byte_Swapping.Swapped2 (PolyORB.Types.Unsigned_Short);
   package DNS_Unsigned_Short is
     new Align_Transfer_Elementary (T => PolyORB.Types.Unsigned_Short);
   procedure Marshall
     (Buffer : access Buffer_Type; Data : PolyORB.Types.Unsigned_Short)
      renames DNS_Unsigned_Short.Marshall;

   function Unmarshall
     (Buffer : access Buffer_Type) return PolyORB.Types.Unsigned_Short
      renames DNS_Unsigned_Short.Unmarshall;

   function Swapped is
     new GNAT.Byte_Swapping.Swapped8 (PolyORB.Types.Unsigned_Long_Long);
   package DNS_Unsigned_Long_Long is
     new Align_Transfer_Elementary (T => PolyORB.Types.Unsigned_Long_Long);

   procedure Marshall
     (Buffer : access Buffer_Type; Data : PolyORB.Types.Unsigned_Long_Long)
      renames DNS_Unsigned_Long_Long.Marshall;

   function Unmarshall
     (Buffer : access Buffer_Type) return PolyORB.Types.Unsigned_Long_Long
      renames DNS_Unsigned_Long_Long.Unmarshall;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"representations.dns",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Representations.DNS;
