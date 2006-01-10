------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E P R E S E N T A T I O N S . C D R           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Any.ObjRef;
with PolyORB.Log;
with PolyORB.Representations.CDR.Common;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Buffers;

package body PolyORB.Representations.CDR is

   use Ada.Streams;
   use PolyORB.Any;
   use PolyORB.Buffers;
   use PolyORB.Errors;
   use PolyORB.Log;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Types;
   use PolyORB.Utils.Buffers;

   package L is new PolyORB.Log.Facility_Log ("polyorb.representations.cdr");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Registry_Item is record
      Major   : Octet;
      Minor   : Octet;
      Factory : CDR_Representation_Factory;
   end record;

   package Factory_Lists is new Utils.Chained_Lists (Registry_Item);

   Factory_Registry : Factory_Lists.List;

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

   ---------------------------
   -- Create_Representation --
   ---------------------------

   function Create_Representation
     (Major : Types.Octet;
      Minor : Types.Octet)
      return CDR_Representation_Access
   is
      use Factory_Lists;

      Iter : Iterator := First (Factory_Registry);
   begin
      while not Last (Iter) loop
         if Value (Iter).Major = Major
           and then Value (Iter).Minor = Minor
         then
            return Value (Iter).Factory.all;
         end if;

         Next (Iter);
      end loop;

      return null;
   end Create_Representation;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer         : access Buffer_Type;
      Representation : CDR_Representation'Class;
      Data           : PolyORB.Any.Any)
   is
      E : Errors.Error_Container;
   begin
      pragma Debug (O ("Marshall (Any): enter"));
      Marshall (Buffer, Representation, Get_Type (Data));
      pragma Debug (O ("Marshall (Any): type marshalled"));
      Marshall_From_Any (Representation, Buffer, Data, E);
      pragma Debug (O ("Marshall (Any): end"));
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer         : access Buffer_Type;
      Representation : CDR_Representation'Class;
      Data           : PolyORB.Any.TypeCode.Object)
   is
      Complex_Buffer : Buffer_Access;
   begin
      pragma Debug (O ("Marshall (Typecode): enter"));
      pragma Debug (O ("Marshall (Typecode): kind is " &
                       TCKind'Image (PolyORB.Any.TypeCode.Kind (Data))));

      case PolyORB.Any.TypeCode.Kind (Data) is

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
            pragma Debug (O ("Marshall (TypeCode): it has "
                             & PolyORB.Types.Unsigned_Long'Image
                             (PolyORB.Any.TypeCode.Parameter_Count (Data))
                             & " parameters"));
            Marshall (Buffer, PolyORB.Any.TypeCode.Id (Data));
            Marshall (Buffer, PolyORB.Any.TypeCode.Name (Data));

         when Tk_Struct =>
            Marshall (Buffer, TC_Struct_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            pragma Debug (O ("Marshall (TypeCode): marshalling the id"));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            pragma Debug (O ("Marshall (TypeCode): marshalling the name"));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               pragma Debug (O ("Marshall (TypeCode): " &
                                "marshalling the members. Nb = "
                                & PolyORB.Types.Unsigned_Long'Image (Nb)));
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     pragma Debug (O ("Marshall (TypeCode): about "
                                      & "to marshall a new  member"));
                     Marshall (Complex_Buffer,
                               PolyORB.Any.TypeCode.Member_Name (Data, J));
                     pragma Debug
                       (O ("Marshall (TypeCode): marshalling "
                           & "the type ("
                           & TCKind'Image
                           (TypeCode.Kind
                            (PolyORB.Any.TypeCode.Member_Type (Data, J)))
                           & ")"));
                     Marshall (Complex_Buffer, Representation,
                               PolyORB.Any.TypeCode.Member_Type (Data, J));
                     pragma Debug (O ("Marshall (TypeCode): "
                                      & "member marshalled"));
                  end loop;
               end if;
            end;
            pragma Debug (O ("Marshall: all members marshalled"));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Union =>
            Marshall (Buffer, TC_Union_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Id (Data));
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Name (Data));
            Marshall
              (Complex_Buffer,
               Representation,
               PolyORB.Any.TypeCode.Discriminator_Type (Data));
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Default_Index (Data));
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
               E : Errors.Error_Container;
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Marshall_From_Any
                       (Representation,
                        Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Label (Data, J),
                        E);
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Name (Data, J));
                     Marshall
                       (Complex_Buffer,
                        Representation,
                        PolyORB.Any.TypeCode.Member_Type (Data, J));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Enum =>
            Marshall (Buffer, TC_Enum_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Marshall (Complex_Buffer,
                               PolyORB.Any.TypeCode.Member_Name (Data, J));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_String =>
            Marshall (Buffer, TC_String_Id);
            pragma Debug (O ("marshall (typecode): " &
                             "about to marshall length: " &
                             PolyORB.Types.Unsigned_Long'Image
                             (PolyORB.Any.TypeCode.Length (Data))));
            Marshall (Buffer, PolyORB.Any.TypeCode.Length (Data));
            pragma Debug (O ("marshall (typecode): length marshalled"));

         when Tk_Sequence =>
            Marshall (Buffer, TC_Sequence_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      Representation,
                      PolyORB.Any.TypeCode.Content_Type (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Length (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Array =>
            Marshall (Buffer, TC_Array_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      Representation,
                      PolyORB.Any.TypeCode.Content_Type (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Length (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Alias =>
            Marshall (Buffer, TC_Alias_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            Marshall (Complex_Buffer,
                      Representation,
                      PolyORB.Any.TypeCode.Content_Type (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Except =>
            Marshall (Buffer, TC_Except_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Marshall (Complex_Buffer,
                               PolyORB.Any.TypeCode.Member_Name (Data, J));
                     Marshall (Complex_Buffer,
                               Representation,
                               PolyORB.Any.TypeCode.Member_Type (Data, J));
                  end loop;
               end if;
            end;
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
            Marshall (Buffer, PolyORB.Any.TypeCode.Length (Data));

         when Tk_Fixed =>
            Marshall (Buffer, TC_Fixed_Id);
            Marshall (Buffer, PolyORB.Any.TypeCode.Fixed_Digits (Data));
            Marshall (Buffer, PolyORB.Any.TypeCode.Fixed_Scale (Data));

         when Tk_Value =>
            Marshall (Buffer, TC_Value_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Id (Data));
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Name (Data));
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Type_Modifier (Data));
            Marshall
              (Complex_Buffer,
               Representation,
               PolyORB.Any.TypeCode.Concrete_Base_Type (Data));
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Name (Data, J));
                     Marshall
                       (Complex_Buffer,
                        Representation,
                        PolyORB.Any.TypeCode.Member_Type (Data, J));
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Visibility (Data, J));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Valuebox =>
            Marshall (Buffer, TC_Valuebox_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            Marshall (Complex_Buffer,
                      Representation,
                      PolyORB.Any.TypeCode.Content_Type (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Native =>
            Marshall (Buffer, TC_Native_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Abstract_Interface =>
            Marshall (Buffer, TC_Abstract_Interface_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Local_Interface =>
            Marshall (Buffer, TC_Local_Interface_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Component =>
            Marshall (Buffer, TC_Component_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Home =>
            Marshall (Buffer, TC_Home_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Id (Data));
            Marshall (Complex_Buffer,
                      PolyORB.Any.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);

         when Tk_Event =>
            Marshall (Buffer, TC_Event_Id);
            Complex_Buffer := new Buffer_Type;
            Start_Encapsulation (Complex_Buffer);
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Id (Data));
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Name (Data));
            Marshall
              (Complex_Buffer,
               PolyORB.Any.TypeCode.Type_Modifier (Data));
            Marshall
              (Complex_Buffer,
               Representation,
               PolyORB.Any.TypeCode.Concrete_Base_Type (Data));
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.TypeCode.Member_Count (Data);
            begin
               Marshall (Complex_Buffer, Nb);
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Name (Data, J));
                     Marshall
                       (Complex_Buffer,
                        Representation,
                        PolyORB.Any.TypeCode.Member_Type (Data, J));
                     Marshall
                       (Complex_Buffer,
                        PolyORB.Any.TypeCode.Member_Visibility (Data, J));
                  end loop;
               end if;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer));
            Release (Complex_Buffer);
      end case;
      pragma Debug (O ("Marshall (Typecode): end"));
   end Marshall;

   -----------------------
   -- Marshall_From_Any --
   -----------------------

   procedure Marshall_From_Any
     (R      : CDR_Representation;
      Buffer : access Buffer_Type;
      Data   : Any.Any;
      Error  : in out Errors.Error_Container)
   is
      Data_Type : constant PolyORB.Any.TypeCode.Object
                    := PolyORB.Any.Get_Unwound_Type (Data);
   begin
      pragma Debug (O ("Marshall_From_Any: enter"));
      pragma Debug (O ("Marshall_From_Any: kind is "
        & TCKind'Image (PolyORB.Any.TypeCode.Kind (Data_Type))));

      case PolyORB.Any.TypeCode.Kind (Data_Type) is

         when Tk_Null | Tk_Void =>
            null;

         when Tk_Short =>
            Marshall (Buffer, PolyORB.Types.Short'(From_Any (Data)));

         when Tk_Long =>
            Marshall (Buffer, PolyORB.Types.Long'(From_Any (Data)));

         when Tk_Ushort =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Short'(From_Any (Data)));

         when Tk_Ulong =>
            Marshall (Buffer, PolyORB.Types.Unsigned_Long'(From_Any (Data)));

         when Tk_Float =>
            Marshall (Buffer, PolyORB.Types.Float'(From_Any (Data)));

         when Tk_Double =>
            Marshall (Buffer, PolyORB.Types.Double'(From_Any (Data)));

         when Tk_Boolean =>
            Marshall (Buffer, PolyORB.Types.Boolean'(From_Any (Data)));

         when Tk_Char =>
            Marshall
              (CDR_Representation'Class (R),
               Buffer,
               PolyORB.Types.Char'(From_Any (Data)),
               Error);

         when Tk_Octet =>
            Marshall (Buffer, PolyORB.Types.Octet'(From_Any (Data)));

         when Tk_Any =>
            Marshall
              (Buffer,
               CDR_Representation'Class (R),
               PolyORB.Any.Any'(From_Any (Data)));

         when Tk_TypeCode =>
            Marshall
              (Buffer,
               CDR_Representation'Class (R),
               PolyORB.Any.TypeCode.Object'(From_Any (Data)));

         when Tk_Principal =>
            --  FIXME: TBD
            raise Program_Error;

         when Tk_Objref =>
            Marshall (Buffer, PolyORB.Any.ObjRef.From_Any (Data));

         when Tk_Struct | Tk_Except =>
            declare
               Nb : constant PolyORB.Types.Unsigned_Long
                 := PolyORB.Any.Get_Aggregate_Count (Data);
            begin
               if Nb /= 0 then
                  declare
                     Value : PolyORB.Any.Any;
                  begin
                     for J in 0 .. Nb - 1 loop
                        Value := PolyORB.Any.Get_Aggregate_Element
                          (Data,
                           PolyORB.Any.TypeCode.Member_Type
                           (Data_Type, J), J);
                        Marshall_From_Any
                          (CDR_Representation'Class (R),
                           Buffer,
                           Value,
                           Error);
                        if Found (Error) then
                           return;
                        end if;
                     end loop;
                  end;
               end if;
            end;

         when Tk_Union =>

            declare
               Label_Value : constant Any.Any
                 := Get_Aggregate_Element
                 (Data,
                  PolyORB.Any.TypeCode.Discriminator_Type (Data_Type),
                  0);
               Member_Value : Any.Any;
            begin
               pragma Assert (PolyORB.Any.Get_Aggregate_Count (Data) = 2);

               Marshall_From_Any
                 (CDR_Representation'Class (R),
                  Buffer,
                  Label_Value,
                  Error);

               if Found (Error) then
                  return;
               end if;

               pragma Debug (O ("Marshall_From_Any: union label "
                                & Any.Image (Label_Value) & " marshalled"));

               Member_Value := Get_Aggregate_Element
                  (Data,
                   PolyORB.Any.TypeCode.Member_Type_With_Label
                   (Data_Type, Label_Value),
                   1);

               Marshall_From_Any
                 (CDR_Representation'Class (R),
                  Buffer,
                  Member_Value,
                  Error);

               if Found (Error) then
                  return;
               end if;

               pragma Debug (O ("Marshall_From_Any: union member value "
                                & Any.Image (Member_Value) & " marshalled"));
            end;

         when Tk_Enum =>
            Marshall_From_Any
              (CDR_Representation'Class (R),
               Buffer,
               PolyORB.Any.Get_Aggregate_Element
                 (Data,
                  PolyORB.Any.TypeCode.TC_Unsigned_Long,
                  PolyORB.Types.Unsigned_Long (0)),
               Error);

            if Found (Error) then
               return;
            end if;

         when Tk_String =>
            Marshall
              (CDR_Representation'Class (R),
               Buffer,
               PolyORB.Types.String'(From_Any (Data)),
               Error);

         when Tk_Sequence =>
            declare
               Nb : constant PolyORB.Types.Unsigned_Long :=
                 PolyORB.Any.Get_Aggregate_Count (Data) - 1;
               Value : PolyORB.Any.Any;
            begin
               Value := PolyORB.Any.Get_Aggregate_Element
                 (Data,
                  PolyORB.Any.TypeCode.TC_Unsigned_Long,
                  PolyORB.Types.Unsigned_Long (0));
               pragma Assert (Nb = From_Any (Value));
               Marshall_From_Any
                 (CDR_Representation'Class (R),
                  Buffer,
                  Value,
                  Error);

               if Found (Error) then
                  return;
               end if;

               for J in 1 .. Nb loop
                  Value := PolyORB.Any.Get_Aggregate_Element
                    (Data,
                     PolyORB.Any.TypeCode.Content_Type (Data_Type),
                     J);
                  Marshall_From_Any
                    (CDR_Representation'Class (R),
                     Buffer,
                     Value,
                     Error);
                  if Found (Error) then
                     return;
                  end if;
               end loop;
            end;

         when Tk_Array =>
            declare
               Nb           : constant PolyORB.Types.Unsigned_Long
                 := PolyORB.Any.TypeCode.Length (Data_Type);
               Value        : PolyORB.Any.Any;
               Content_Type : constant PolyORB.Any.TypeCode.Object
                 := PolyORB.Any.Unwind_Typedefs
                    (PolyORB.Any.TypeCode.Content_Type (Data_Type));

            begin
               for J in 1 .. Nb loop
                  Value :=
                    PolyORB.Any.Get_Aggregate_Element
                    (Data,
                     Content_Type,
                     J - 1);
                  pragma Debug (O ("Marshall_From_Any: value kind is "
                                   & PolyORB.Any.TCKind'Image
                                   (PolyORB.Any.TypeCode.Kind
                                    (PolyORB.Any.Get_Unwound_Type (Value)))));
                  Marshall_From_Any
                    (CDR_Representation'Class (R),
                     Buffer,
                     Value,
                     Error);

                  if Found (Error) then
                     return;
                  end if;
               end loop;
            end;

         when Tk_Alias =>
            --  Should never happen
            raise Program_Error;

         when Tk_Longlong =>
            Marshall (Buffer, PolyORB.Types.Long_Long'(From_Any (Data)));

         when Tk_Ulonglong =>
            Marshall (Buffer,
              PolyORB.Types.Unsigned_Long_Long'(From_Any (Data)));

         when Tk_Longdouble =>
            Marshall (Buffer, PolyORB.Types.Long_Double'(From_Any (Data)));

         when Tk_Widechar =>
            Marshall
              (CDR_Representation'Class (R),
               Buffer,
               PolyORB.Types.Wchar'(From_Any (Data)),
               Error);

         when Tk_Wstring =>
            Marshall
              (CDR_Representation'Class (R),
               Buffer,
               PolyORB.Types.Wide_String'(From_Any (Data)),
               Error);

         when Tk_Fixed =>
            declare
               B : Stream_Element_Array (1 ..
                 Stream_Element_Offset
                   (TypeCode.Fixed_Digits (Data_Type)));
               Last : Stream_Element_Offset := B'First;
            begin
               loop
                  B (Last) := Stream_Element (Octet'(
                    From_Any (Get_Aggregate_Element (Data,
                      PolyORB.Any.TypeCode.TC_Octet,
                      PolyORB.Types.Unsigned_Long (Last - B'First)))));
                  exit when B (Last) mod 16 > 9;
                  Last := Last + 1;
               end loop;
               Align_Marshall_Copy (Buffer, B (B'First .. Last));
            end;

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
               --    PolyORB.Any.TypeCode.Member_Type (Data_Type, I + 3 * J),
               --        J);
               --     declare
               --        Member_Type : constant PolyORB.Any.TypeCode.Object
               --          := PolyORB.Any.Get_Unwound_Type (Member_Value);
               --     begin
               --        case PolyORB.Any.TypeCode.Kind (Member_Type) is
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
            Marshall_From_Any
              (CDR_Representation'Class (R),
               Buffer,
               PolyORB.Any.Get_Aggregate_Element
                 (Data, PolyORB.Any.TypeCode.Member_Type (Data_Type,
                  PolyORB.Types.Unsigned_Long (0)),
                  PolyORB.Types.Unsigned_Long (0)),
               Error);

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
      pragma Debug (O ("Marshall_From_Any: end"));
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

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer         : access Buffer_Type;
      Representation : CDR_Representation'Class)
      return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any;
      Tc     : constant PolyORB.Any.TypeCode.Object
        := Unmarshall (Buffer, Representation);
      E      : Errors.Error_Container;
   begin
      pragma Debug (O ("Unmarshall (Any): enter"));
      Result := Get_Empty_Any (Tc);
      Unmarshall_To_Any (Representation, Buffer, Result, E);
      pragma Debug (O ("Unmarshall (Any): end"));
      return Result;
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer         : access Buffer_Type;
      Representation : CDR_Representation'Class)
     return PolyORB.Any.TypeCode.Object
   is
      TypeCode_Id : constant PolyORB.Types.Unsigned_Long
        := Unmarshall (Buffer);
      Result      : PolyORB.Any.TypeCode.Object;
   begin
      pragma Debug (O ("Unmarshall (TypeCode): enter"));

      case TypeCode_Id is
         when TC_Null_Id =>
            Result := PolyORB.Any.TypeCode.TC_Null;

         when TC_Void_Id =>
            Result := PolyORB.Any.TypeCode.TC_Void;

         when TC_Short_Id =>
            Result := PolyORB.Any.TypeCode.TC_Short;

         when TC_Long_Id =>
            Result := PolyORB.Any.TypeCode.TC_Long;

         when TC_Unsigned_Short_Id =>
            Result := PolyORB.Any.TypeCode.TC_Unsigned_Short;

         when TC_Unsigned_Long_Id =>
            Result := PolyORB.Any.TypeCode.TC_Unsigned_Long;

         when TC_Float_Id =>
            Result := PolyORB.Any.TypeCode.TC_Float;

         when TC_Double_Id =>
            Result := PolyORB.Any.TypeCode.TC_Double;

         when TC_Boolean_Id =>
            Result := PolyORB.Any.TypeCode.TC_Boolean;

         when TC_Char_Id =>
            Result := PolyORB.Any.TypeCode.TC_Char;

         when TC_Octet_Id =>
            Result := PolyORB.Any.TypeCode.TC_Octet;

         when TC_Any_Id =>
            Result := PolyORB.Any.TypeCode.TC_Any;

         when TC_TypeCode_Id =>
            Result := PolyORB.Any.TypeCode.TC_TypeCode;

         when TC_Principal_Id =>
            Result := PolyORB.Any.TypeCode.TC_Principal;

         when TC_Object_Id =>
            Result := PolyORB.Any.TypeCode.TC_Object;
            declare
               Id   : constant PolyORB.Types.String
                 := Types.String (Types.RepositoryId'(Unmarshall (Buffer)));
               Name : constant PolyORB.Types.String
                 := Types.String (Types.RepositoryId'(Unmarshall (Buffer)));
            begin
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;

         when TC_Struct_Id =>
            Result := PolyORB.Any.TypeCode.TC_Struct;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : PolyORB.Types.String;
               Nb : PolyORB.Types.Unsigned_Long;
               Member_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               Nb   := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name :=
                       Types.String
                         (Types.Identifier'
                            (Unmarshall (Complex_Buffer'Access)));
                     Member_Type :=
                       Unmarshall (Complex_Buffer'Access, Representation);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;

         when TC_Union_Id =>
            Result := PolyORB.Any.TypeCode.TC_Union;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : PolyORB.Types.String;
               Nb, Default_Index : PolyORB.Types.Unsigned_Long;
               Discriminator_Type, Member_Type : PolyORB.Any.TypeCode.Object;
               Member_Label : PolyORB.Any.Any;
               E : Error_Container;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               Discriminator_Type :=
                 Unmarshall (Complex_Buffer'Access, Representation);
               Default_Index := Unmarshall (Complex_Buffer'Access);
               Nb := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Discriminator_Type));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Default_Index));
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Label := Get_Empty_Any (Discriminator_Type);
                     Unmarshall_To_Any
                       (Representation,
                        Complex_Buffer'Access,
                        Member_Label,
                        E);
                     Member_Name :=
                       Types.String
                         (Types.Identifier'
                            (Unmarshall (Complex_Buffer'Access)));
                     Member_Type :=
                       Unmarshall (Complex_Buffer'Access, Representation);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, Member_Label);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;

         when TC_Enum_Id =>
            Result := PolyORB.Any.TypeCode.TC_Enum;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
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
               Nb := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name :=
                       Types.String
                         (Types.Identifier'
                            (Unmarshall (Complex_Buffer'Access)));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;

         when TC_String_Id =>
            Result := PolyORB.Any.TypeCode.TC_String;
            declare
               Length : PolyORB.Types.Unsigned_Long;
            begin
               Length := Unmarshall (Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;

         when TC_Sequence_Id =>
            Result := PolyORB.Any.TypeCode.TC_Sequence;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Length : PolyORB.Types.Unsigned_Long;
               Content_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Content_Type :=
                 Unmarshall (Complex_Buffer'Access, Representation);
               Length := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;

         when TC_Array_Id =>
            Result := PolyORB.Any.TypeCode.TC_Array;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Length : PolyORB.Types.Unsigned_Long;
               Content_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Content_Type :=
                 Unmarshall (Complex_Buffer'Access, Representation);
               Length := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;

         when TC_Alias_Id =>
            Result := PolyORB.Any.TypeCode.TC_Alias;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : PolyORB.Types.String;
               Content_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               Content_Type :=
                 Unmarshall (Complex_Buffer'Access, Representation);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;

         when TC_Except_Id =>
            Result := PolyORB.Any.TypeCode.TC_Except;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : PolyORB.Types.String;
               Nb : PolyORB.Types.Unsigned_Long;
               Member_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               Nb := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name :=
                       Types.String
                         (Types.Identifier'
                            (Unmarshall (Complex_Buffer'Access)));
                     Member_Type :=
                       Unmarshall (Complex_Buffer'Access, Representation);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;

         when TC_Long_Long_Id =>
            Result := PolyORB.Any.TypeCode.TC_Long_Long;

         when TC_Unsigned_Long_Long_Id =>
            Result := PolyORB.Any.TypeCode.TC_Unsigned_Long_Long;

         when TC_Long_Double_Id =>
            Result := PolyORB.Any.TypeCode.TC_Long_Double;

         when TC_Wchar_Id =>
            Result := PolyORB.Any.TypeCode.TC_Wchar;

         when TC_Wide_String_Id =>
            Result := PolyORB.Any.TypeCode.TC_Wide_String;
            declare
               Length : PolyORB.Types.Unsigned_Long;
            begin
               Length := Unmarshall (Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;

         when TC_Fixed_Id =>
            Result := PolyORB.Any.TypeCode.TC_Fixed;
            declare
               Fixed_Digits : PolyORB.Types.Unsigned_Short;
               Fixed_Scale : PolyORB.Types.Short;
            begin
               Fixed_Digits := Unmarshall (Buffer);
               Fixed_Scale := Unmarshall (Buffer);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Fixed_Digits));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Fixed_Scale));
            end;

         when TC_Value_Id =>
            Result := PolyORB.Any.TypeCode.TC_Value;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : PolyORB.Types.String;
               Type_Modifier, Visibility : PolyORB.Types.Short;
               Nb : PolyORB.Types.Unsigned_Long;
               Concrete_Base_Type, Member_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               Type_Modifier := Unmarshall (Complex_Buffer'Access);
               Concrete_Base_Type :=
                 Unmarshall (Complex_Buffer'Access, Representation);
               Nb := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Type_Modifier));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Concrete_Base_Type));
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name :=
                       Types.String
                         (Types.Identifier'
                            (Unmarshall (Complex_Buffer'Access)));
                     Member_Type :=
                       Unmarshall (Complex_Buffer'Access, Representation);
                     Visibility := Unmarshall (Complex_Buffer'Access);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Visibility));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;

         when TC_Valuebox_Id =>
            Result := PolyORB.Any.TypeCode.TC_Valuebox;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : PolyORB.Types.String;
               Content_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               Content_Type :=
                 Unmarshall (Complex_Buffer'Access, Representation);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;

         when TC_Native_Id =>
            Result := PolyORB.Any.TypeCode.TC_Native;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;

         when TC_Abstract_Interface_Id =>
            Result := PolyORB.Any.TypeCode.TC_Abstract_Interface;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;

         when TC_Local_Interface_Id =>
            Result := PolyORB.Any.TypeCode.TC_Local_Interface;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;

         when TC_Component_Id =>
            Result := PolyORB.Any.TypeCode.TC_Component;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;

         when TC_Home_Id =>
            Result := PolyORB.Any.TypeCode.TC_Home;
            declare
               Complex_Encap :  aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : PolyORB.Types.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;

         when TC_Event_Id =>
            Result := PolyORB.Any.TypeCode.TC_Event;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : PolyORB.Types.String;
               Type_Modifier, Visibility : PolyORB.Types.Short;
               Nb : PolyORB.Types.Unsigned_Long;
               Concrete_Base_Type, Member_Type : PolyORB.Any.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id :=
                 Types.String
                   (Types.RepositoryId'(Unmarshall (Complex_Buffer'Access)));
               Name :=
                 Types.String
                   (Types.Identifier'(Unmarshall (Complex_Buffer'Access)));
               Type_Modifier := Unmarshall (Complex_Buffer'Access);
               Concrete_Base_Type :=
                 Unmarshall (Complex_Buffer'Access, Representation);
               Nb := Unmarshall (Complex_Buffer'Access);
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Type_Modifier));
               PolyORB.Any.TypeCode.Add_Parameter
                 (Result, To_Any (Concrete_Base_Type));
               if Nb /= 0 then
                  for J in 0 .. Nb - 1 loop
                     Member_Name :=
                       Types.String
                         (Types.Identifier'
                            (Unmarshall (Complex_Buffer'Access)));
                     Member_Type :=
                       Unmarshall (Complex_Buffer'Access, Representation);
                     Visibility := Unmarshall (Complex_Buffer'Access);
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Visibility));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Type));
                     PolyORB.Any.TypeCode.Add_Parameter
                       (Result, To_Any (Member_Name));
                  end loop;
               end if;
            end;

         when others =>
            raise Constraint_Error;
      end case;

      pragma Debug (O ("Unmarshall (TypeCode): end"));
      return Result;
   end Unmarshall;

   -----------------------
   -- Unmarshall_To_Any --
   -----------------------

   procedure Unmarshall_To_Any
     (R      : CDR_Representation;
      Buffer : access Buffer_Type;
      Data   : in out Any.Any;
      Error  : in out Errors.Error_Container)
   is
      Tc : constant PolyORB.Any.TypeCode.Object := Get_Unwound_Type (Data);
   begin
      pragma Debug (O ("Unmarshall_To_Any: enter"));
      pragma Debug
        (O ("Unmarshall_To_Any: Any_Type is " &
            PolyORB.Any.TCKind'Image (TypeCode.Kind (Tc))));

      case TypeCode.Kind (Tc) is

         when Tk_Null | Tk_Void =>
            null;

         when Tk_Short =>
            declare
               S : constant Short := Unmarshall (Buffer);
            begin
               pragma Debug (O ("Unmarshall_To_Any: value is "
                                & PolyORB.Types.Short'Image (S)));
               Set_Any_Value (Data, S);
            end;

         when Tk_Long =>
            declare
               L : constant Long := Unmarshall (Buffer);
            begin
               Set_Any_Value (Data, L);
            end;

         when Tk_Ushort =>
            declare
               Us : constant Unsigned_Short := Unmarshall (Buffer);
            begin
               Set_Any_Value (Data, Us);
            end;

         when Tk_Ulong =>
            declare
               Ul : constant Unsigned_Long := Unmarshall (Buffer);
            begin
               Set_Any_Value (Data, Ul);
            end;

         when Tk_Float =>
            declare
               F : constant PolyORB.Types.Float := Unmarshall (Buffer);
            begin
               Set_Any_Value (Data, F);
            end;

         when Tk_Double =>
            declare
               D : constant Double := Unmarshall (Buffer);
            begin
               pragma Debug
                 (O ("Unmarshall_To_Any: value is " & Double'Image (D)));
               Set_Any_Value (Data, D);
            end;

         when Tk_Boolean =>
            declare
               B : constant PolyORB.Types.Boolean := Unmarshall (Buffer);
            begin
               Set_Any_Value (Data, B);
            end;

         when Tk_Char =>
            declare
               C : Char;
            begin
               Unmarshall (CDR_Representation'Class (R), Buffer, C, Error);
               Set_Any_Value (Data, C);
            end;

         when Tk_Octet =>
            declare
               O : constant PolyORB.Types.Octet := Unmarshall (Buffer);
            begin
               Set_Any_Value (Data, O);
            end;

         when Tk_Any =>
            declare
               A : constant Any.Any
                 := Unmarshall (Buffer, CDR_Representation'Class (R));
            begin
               Set_Any_Value (Data, A);
            end;

         when Tk_TypeCode =>
            declare
               T : constant TypeCode.Object
                 := Unmarshall (Buffer, CDR_Representation'Class (R));
            begin
               Set_Any_Value (Data, T);
            end;

         when Tk_Principal =>
            --  FIXME : to be done
            raise Program_Error;

         when Tk_Objref =>
            PolyORB.Any.ObjRef.Set_Any_Value
              (Data, Unmarshall (Buffer));

         when Tk_Struct | Tk_Except =>
            declare
               Nb : constant Unsigned_Long := TypeCode.Member_Count (Tc);
               Element_TC : TypeCode.Object;
               Val : PolyORB.Any.Any;

               Add_Elements : Boolean := True;

            begin
               if Is_Empty (Data) then
                  Move_Any_Value (Data,
                    Get_Empty_Any_Aggregate (Get_Type (Data)));
               elsif Get_Aggregate_Count (Data) /= 0 then
                  pragma Assert (Get_Aggregate_Count (Data) = Nb);
                  Add_Elements := False;
               end if;

               for J in 1 .. Nb loop
                  pragma Debug (O ("Unmarshall_To_Any: get the element"));
                  Element_TC := TypeCode.Member_Type (Tc, J - 1);
                  if Add_Elements then
                     Val := Get_Empty_Any (Element_TC);
                     Add_Aggregate_Element (Data, Val);
                  else
                     Val := Get_Aggregate_Element (Data, Element_TC, J - 1);
                  end if;

                  pragma Debug (O ("Unmarshall_To_Any: about to "
                                   & "unmarshall a parameter"));
                  Unmarshall_To_Any
                    (CDR_Representation'Class (R),
                     Buffer,
                     Val,
                     Error);

                  if Found (Error) then
                     return;
                  end if;
               end loop;
            end;

         when Tk_Union =>
            declare
               Label, Val : PolyORB.Any.Any;

            begin
               Move_Any_Value (Data,
                  Get_Empty_Any_Aggregate (Get_Type (Data)));
               Label := Get_Empty_Any (TypeCode.Discriminator_Type (Tc));
               Unmarshall_To_Any
                 (CDR_Representation'Class (R),
                  Buffer,
                  Label,
                  Error);

               if Found (Error) then
                  return;
               end if;

               declare
                  Member_TC : constant TypeCode.Object :=
                                TypeCode.Member_Type_With_Label (Tc, Label);
               begin
                  Val := Get_Empty_Any (Member_TC);
                  Unmarshall_To_Any
                    (CDR_Representation'Class (R),
                     Buffer,
                     Val,
                     Error);
               end;

               if Found (Error) then
                  return;
               end if;

               Add_Aggregate_Element (Data, Label);
               Add_Aggregate_Element (Data, Val);
            end;

         when Tk_Enum =>
            declare
               Val : PolyORB.Any.Any
                 := Get_Empty_Any (TC_Unsigned_Long);
               Initial_Count : Unsigned_Long;
            begin
               if Is_Empty (Data) then
                  Move_Any_Value (Data,
                    Get_Empty_Any_Aggregate (Get_Type (Data)));
                  Initial_Count := 0;
               else
                  Initial_Count := Get_Aggregate_Count (Data);
               end if;

               if Initial_Count = 0 then
                  Val := Get_Empty_Any (TC_Unsigned_Long);
                  Add_Aggregate_Element (Data, Val);
               else
                  pragma Assert (Initial_Count = 1);
                  Val := Get_Aggregate_Element (Data, TC_Unsigned_Long, 0);
               end if;

               Unmarshall_To_Any
                 (CDR_Representation'Class (R),
                  Buffer,
                  Val,
                  Error);

               if Found (Error) then
                  return;
               end if;

               pragma Debug
                 (O ("enum -> ["
                     & Utils.Trimmed_Image
                     (Integer (Types.Unsigned_Long'(From_Any (Val))))
                     & "] "
                     & Types.To_Standard_String
                     (TypeCode.Enumerator_Name (Tc, From_Any (Val)))));
            end;

         when Tk_String =>
            declare
               S : PolyORB.Types.String;
            begin
               Unmarshall
                 (CDR_Representation'Class (R),
                  Buffer,
                  S,
                  Error);
               Set_Any_Value (Data, S);
            end;

         when Tk_Sequence =>
            declare
               Len        : constant Unsigned_Long   := Unmarshall (Buffer);
               Max_Len    : constant Unsigned_Long   := TypeCode.Length (Tc);
               Element_TC : constant TypeCode.Object :=
                              TypeCode.Content_Type (Tc);

               Val : PolyORB.Any.Any;

               Add_Elements : Boolean;

            begin
               if Max_Len > 0 and then Len > Max_Len then
                  raise Constraint_Error;
               end if;

               if Is_Empty (Data)
                 or else Get_Aggregate_Count (Data) /= Len + 1
               then
                  Move_Any_Value (Data,
                    Get_Empty_Any_Aggregate (Get_Type (Data)));
                  Add_Elements := True;
                  Add_Aggregate_Element (Data, To_Any (Len));
               else
                  Add_Elements := False;
               end if;

               pragma Debug
                 (O ("Unmarshall_To_Any: unmarshalling"
                     & Unsigned_Long'Image (Len) & " elements, "
                     & "Add_Elements = " & Boolean'Image (Add_Elements)));

               for J in 1 .. Len loop
                  if Add_Elements then
                     Val := Get_Empty_Any (Element_TC);
                     Add_Aggregate_Element (Data, Val);
                  else
                     Val := Get_Aggregate_Element (Data, Element_TC, J);
                  end if;
                  Unmarshall_To_Any
                    (CDR_Representation'Class (R),
                     Buffer,
                     Val,
                     Error);

                  if Found (Error) then
                     return;
                  end if;
               end loop;
               pragma Debug (O ("Unmarshalled sequence."));
            end;

         when Tk_Array =>
            declare
               Nb           : constant PolyORB.Types.Unsigned_Long
                 := PolyORB.Any.TypeCode.Length (Tc);
               Element_Type : constant PolyORB.Any.TypeCode.Object
                 := PolyORB.Any.TypeCode.Content_Type (Tc);
               Add_Elements : Boolean := True;
               Val : PolyORB.Any.Any;

            begin
               if Is_Empty (Data) then
                  Move_Any_Value
                    (Data,
                     Get_Empty_Any_Aggregate (Get_Type (Data)));

               elsif Get_Aggregate_Count (Data) /= 0 then
                  pragma Assert (Get_Aggregate_Count (Data) = Nb);
                  Add_Elements := False;
               end if;

               for J in 1 .. Nb loop
                  if Add_Elements then
                     Val := Get_Empty_Any (Element_Type);
                     Add_Aggregate_Element (Data, Val);

                  else
                     Val := Get_Aggregate_Element (Data, Element_Type, J - 1);
                  end if;

                  Unmarshall_To_Any
                    (CDR_Representation'Class (R),
                     Buffer,
                     Val,
                     Error);

                  if Found (Error) then
                     return;
                  end if;
               end loop;

               pragma Debug (O ("Unmarshall_To_Any: array done."));
            end;

         when Tk_Alias =>
            --  We should never reach this point
            raise Program_Error;

         when Tk_Longlong =>
            declare
               Ll : constant Long_Long := Unmarshall (Buffer);
            begin
               Set_Any_Value (Data, Ll);
            end;

         when Tk_Ulonglong =>
            declare
               Ull : constant Unsigned_Long_Long := Unmarshall (Buffer);
            begin
               Set_Any_Value (Data, Ull);
            end;

         when Tk_Longdouble =>
            declare
               Ld : constant Long_Double := Unmarshall (Buffer);
            begin
               Set_Any_Value (Data, Ld);
            end;

         when Tk_Widechar =>
            declare
               Wc : Wchar;
            begin
               Unmarshall
                 (CDR_Representation'Class (R),
                  Buffer,
                  Wc,
                  Error);
               Set_Any_Value (Data, Wc);
            end;

         when Tk_Wstring =>
            declare
               Ws : PolyORB.Types.Wide_String;
            begin
               Unmarshall
                 (CDR_Representation'Class (R),
                  Buffer,
                  Ws,
                  Error);
               Set_Any_Value (Data, Ws);
            end;

         when Tk_Fixed =>
            declare
               B : PolyORB.Types.Octet;
            begin
               Move_Any_Value (Data,
                 Get_Empty_Any_Aggregate (Get_Type (Data)));
               loop
                  B := Unmarshall (Buffer);
                  Add_Aggregate_Element (Data, To_Any (B));
                  exit when B mod 16 > 9;
               end loop;
            end;

         when Tk_Value =>

            --  declare
            --   Val_Modifier,Arg: PolyORB.Any.Any;
            --   Nb: PolyORB.Types.Unsigned_Long:=
            --          TypeCode.Member_Count(Tc);

            --  begin
            --   Set_Any_Aggregate_Value(Result);
            --   if Is_Empty then
            --     Val_Modifier:= Get_Empty_Any(TypeCode.Type_Modifier(Tc));
            --   else
            --     Val_Modifier:= Get_Aggregate_Element
            --               (Result,
            --                TypeCode.Discriminator_Type(Tc),
            --                PolyORB.Types.Unsigned_Long(0));
            --   end if;
            --   Unmarshall_To_Any(Buffer,Val_Modifier);
            --   if Is_Empty then
            --     Add_Aggregate_Element(Result,Val_Modifier);
            --   end if;

            --   if Nb /=0 then
            --    for I in 0 .. Nb-1 loop
            --     if Is_Empty then
            --        Arg:= Get_Empty_Any( TypeCode.Member_Visibility(Tc));
            --     else
            --        Arg:= Get_Aggregate_Element
            --               (Result,
            --                TypeCode.Member_Visibility(Tc,I+1),
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
            --  declare
            --     Arg: Corba.Any;
            --  begin
            --     Set_Any_Aggregate_Value(Result);
            --     if Is_Empty then
            --       Arg:= Get_Empty_Any(TypeCode.Member_Type
            --              (Tc,PolyORB.Types.Unsigned_Long(0)));
            --     else
            --       Arg:= PolyORB.Any.Get_Aggregate_Element
            --                 (Result,
            --                  PolyORB.Any.TypeCode.Member_Type(Tc,
            --                  PolyORB.Types.Unsigned_Long(0)));
            --     end if;
            --     Unmarshall_To_Any(Buffer,Arg);
            --     if Is_Empty then
            --       Add_Aggregate_Element(Result, Arg);
            --     end if;
            --  end;
            raise Program_Error;

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
      pragma Debug (O ("Unmarshall_To_Any: end"));
   end Unmarshall_To_Any;

end PolyORB.Representations.CDR;
