------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    C O R B A . F I X E D _ P O I N T                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

with Ada.Unchecked_Deallocation;

with PolyORB.Log;
with PolyORB.Representations.CDR.Common;

package body CORBA.Fixed_Point is

   use PolyORB.Log;

   -----------
   -- Debug --
   -----------

   package L is new PolyORB.Log.Facility_Log ("corba.fixed_point");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   ---------------------
   -- this is to help --
   ---------------------

   package CDR_Fixed_F is
      new PolyORB.Representations.CDR.Common.Fixed_Point (F);

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : F) return CORBA.Any is
      Tco : CORBA.TypeCode.Object;

   begin
      CORBA.TypeCode.Internals.Set_Kind (Tco, PolyORB.Any.Tk_Fixed);
      CORBA.TypeCode.Internals.Add_Parameter
        (Tco, CORBA.To_Any (CORBA.Unsigned_Short (F'Digits)));
      CORBA.TypeCode.Internals.Add_Parameter
        (Tco, CORBA.To_Any (CORBA.Short (F'Scale)));

      declare
         Result : Any := CORBA.Internals.Get_Empty_Any_Aggregate (Tco);
         Octets : constant Ada.Streams.Stream_Element_Array
           := CDR_Fixed_F.Fixed_To_Octets (Item);

      begin
         for I in Octets'Range loop
            CORBA.Internals.Add_Aggregate_Element
              (Result,
               CORBA.To_Any (CORBA.Octet (Octets (I))));
         end loop;

         return Result;
      end;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any) return F is
      use type PolyORB.Any.TCKind;

   begin
      pragma Debug (O ("From_Any (Fixed) : enter"));
      if TypeCode.Kind (Internals.Get_Unwound_Type (Item))
        /= PolyORB.Any.Tk_Fixed
      then
         pragma Debug
           (O ("From_Any (Fixed) : Bad_TypeCode, type is " &
               CORBA.TCKind'Image
               (TypeCode.Kind (Internals.Get_Unwound_Type (Item)))));
         raise Bad_TypeCode;
      end if;

      declare
         use Ada.Streams;

         Nb : constant CORBA.Unsigned_Long :=
           CORBA.Internals.Get_Aggregate_Count (Item);
         Octets : Stream_Element_Array (1 .. Stream_Element_Offset (Nb)) :=
           (others => 0);
         Element : CORBA.Any;
      begin
         for I in Octets'Range loop
            pragma Debug (O ("From_Any (Fixed) : yet another octet"));
            Element :=
              CORBA.Internals.Get_Aggregate_Element
              (Item,
               CORBA.TC_Octet,
               CORBA.Unsigned_Long (I - 1));
            Octets (I) := Stream_Element
              (CORBA.Octet'(CORBA.From_Any (Element)));
         end loop;
         pragma Debug (O ("From_Any (Fixed) : return"));
         return CDR_Fixed_F.Octets_To_Fixed (Octets);

      exception when CORBA.Marshal =>
         pragma Debug (O ("From_Any (Fixed) : exception catched" &
                          "while returning"));
         raise CORBA.Bad_TypeCode;
      end;
   end From_Any;

   ----------
   -- Wrap --
   ----------

   function Wrap (X : access F) return PolyORB.Any.Content'Class is
   begin
      return Fixed_Content'
        (PolyORB.Any.Aggregate_Content with
           V          => X.all'Unrestricted_Access,
           Repr_Cache => (others => 0));
   end Wrap;

   -----------
   -- Clone --
   -----------

   function Clone (ACC : Fixed_Content) return PolyORB.Any.Content_Ptr is
   begin
      return new Fixed_Content'
        (PolyORB.Any.Aggregate_Content with
           V => ACC.V, Repr_Cache => (others => 0));
   end Clone;

   --------------------
   -- Finalize_Value --
   --------------------

   procedure Finalize_Value (ACC : in out Fixed_Content) is
      procedure Free is new Ada.Unchecked_Deallocation (F, F_Ptr);
   begin
      Free (ACC.V);
   end Finalize_Value;

   ---------------------------
   -- Get_Aggregate_Element --
   ---------------------------

   function Get_Aggregate_Element
     (ACC   : access Fixed_Content;
      TC    : PolyORB.Any.TypeCode.Object;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class
   is
      pragma Unreferenced (TC);
      use Ada.Streams;
   begin

      --  If getting first element, prime cache

      if Stream_Element_Offset (Index) = ACC.Repr_Cache'First then
         ACC.Repr_Cache := CDR_Fixed_F.Fixed_To_Octets (ACC.V.all);
      end if;

      Mech.all := PolyORB.Any.By_Reference;
      return PolyORB.Any.Wrap
        (PolyORB.Types.Octet
          (ACC.Repr_Cache
           (Stream_Element_Offset (Index)))'Unrestricted_Access);
   end Get_Aggregate_Element;

   ---------------------------
   -- Set_Aggregate_Element --
   ---------------------------

   procedure Set_Aggregate_Element
     (ACC    : in out Fixed_Content;
      TC     : PolyORB.Any.TypeCode.Object;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : PolyORB.Any.Any_Container_Ptr)
   is
      pragma Unreferenced (TC);
      use Ada.Streams;
   begin
      ACC.Repr_Cache (Stream_Element_Offset (Index)) :=
        Stream_Element
          (PolyORB.Types.Octet'(PolyORB.Any.From_Any (From_C.all)));

      --  If setting last element, update actual fixed value

      if Stream_Element_Offset (Index) = ACC.Repr_Cache'Last then
         ACC.V.all := CDR_Fixed_F.Octets_To_Fixed (ACC.Repr_Cache);
      end if;
   end Set_Aggregate_Element;

   -------------------------
   -- Get_Aggregate_Count --
   -------------------------

   function Get_Aggregate_Count
     (ACC : Fixed_Content) return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (ACC);
   begin
      return Fixed_Content_Count;
   end Get_Aggregate_Count;

   -------------------------
   -- Set_Aggregate_Count --
   -------------------------

   procedure Set_Aggregate_Count
     (ACC   : in out Fixed_Content;
      Count : PolyORB.Types.Unsigned_Long)
   is
      pragma Unreferenced (ACC);
   begin
      if Count /= Fixed_Content_Count then
         raise Constraint_Error;
      end if;
   end Set_Aggregate_Count;

end CORBA.Fixed_Point;
