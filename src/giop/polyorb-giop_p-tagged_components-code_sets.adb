------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.GIOP_P.TAGGED_COMPONENTS.CODE_SETS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;
with PolyORB.Representations.CDR.Common;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Tagged_Components.Code_Sets is

   use PolyORB.Representations.CDR.Common;
   use PolyORB.GIOP_P.Code_Sets;

   function Create_Empty_Component return Tagged_Component_Access;

   function Fetch_Component
     (Oid : access PolyORB.Objects.Object_Id)
      return Tagged_Component_Access;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : Code_Set_Component);

   function Unmarshall
     (Buffer : access Buffer_Type)
      return Code_Set_Component;

   ----------------------------
   -- Create_Empty_Component --
   ----------------------------

   function Create_Empty_Component return Tagged_Component_Access is
   begin
      return new TC_Code_Sets;
   end Create_Empty_Component;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate (C : TC_Code_Sets) return Tagged_Component_Access is
      Result : constant Tagged_Component_Access := new TC_Code_Sets;

   begin
      TC_Code_Sets (Result.all).For_Char_Data :=
        (C.For_Char_Data.Native_Code_Set,
         Duplicate (C.For_Char_Data.Conversion_Code_Sets));
      TC_Code_Sets (Result.all).For_Wchar_Data :=
        (C.For_Wchar_Data.Native_Code_Set,
         Duplicate (C.For_Wchar_Data.Conversion_Code_Sets));

      return Result;
   end Duplicate;

   ---------------------
   -- Fetch_Component --
   ---------------------

   function Fetch_Component
     (Oid : access PolyORB.Objects.Object_Id)
      return Tagged_Component_Access
   is
      pragma Unreferenced (Oid);

      Aux : TC_Code_Sets;

   begin
      Aux.For_Char_Data :=
       (Native_Char_Code_Set, Duplicate (Conversion_Char_Code_Sets));
      Aux.For_Wchar_Data :=
       (Native_Wchar_Code_Set, Duplicate (Conversion_Wchar_Code_Sets));

      return new TC_Code_Sets'(Aux);
   end Fetch_Component;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (C : access TC_Code_Sets) is
   begin
      Deallocate (C.For_Char_Data.Conversion_Code_Sets);
      Deallocate (C.For_Wchar_Data.Conversion_Code_Sets);
   end Release_Contents;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : Code_Set_Component)
   is
      use Code_Set_Id_Lists;

      Iter : Code_Set_Id_Lists.Iterator;

   begin
      Marshall (Buffer, Types.Unsigned_Long (Data.Native_Code_Set));

      Marshall
       (Buffer,
        Types.Unsigned_Long (Length (Data.Conversion_Code_Sets)));

      Iter := First (Data.Conversion_Code_Sets);

      while not Last (Iter) loop
         Marshall (Buffer, Types.Unsigned_Long (Value (Iter).all));
         Next (Iter);
      end loop;
   end Marshall;

   -----------------------------
   -- Marshall_Component_Data --
   -----------------------------

   procedure Marshall_Component_Data
     (C      : access TC_Code_Sets;
      Buffer : access Buffer_Type)
   is
      Temp_Buf : Buffer_Access := new Buffer_Type;

   begin
      --  The body of a Tag_Policy component is an encapsulation

      Start_Encapsulation (Temp_Buf);

      Marshall (Temp_Buf, C.For_Char_Data);
      Marshall (Temp_Buf, C.For_Wchar_Data);

      Marshall (Buffer, Encapsulate (Temp_Buf));
      Release (Temp_Buf);
   end Marshall_Component_Data;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer : access Buffer_Type)
      return Code_Set_Component
   is
      Result : Code_Set_Component;
      Length : Types.Unsigned_Long;

   begin
      Result.Native_Code_Set := Code_Set_Id
        (Types.Unsigned_Long'(Unmarshall (Buffer)));

      Length := Unmarshall (Buffer);

      for J in 1 .. Length loop
         Append (Result.Conversion_Code_Sets,
                 Code_Set_Id (Types.Unsigned_Long'(Unmarshall (Buffer))));
      end loop;

      return Result;
   end Unmarshall;

   -------------------------------
   -- Unmarshall_Component_Data --
   -------------------------------

   procedure Unmarshall_Component_Data
     (C      : access TC_Code_Sets;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use PolyORB.Errors;

      Tag_Body : aliased Encapsulation := Unmarshall (Buffer);

      Temp_Buf : Buffer_Access := new Buffer_Type;
   begin
      Decapsulate (Tag_Body'Access, Temp_Buf);

      C.For_Char_Data := Unmarshall (Temp_Buf);
      C.For_Wchar_Data := Unmarshall (Temp_Buf);

      pragma Assert (Remaining (Temp_Buf) = 0);
      Release (Temp_Buf);

   exception
      when others =>
               Release (Temp_Buf);
               Throw (Error,
                      Bad_Param_E,
                      System_Exception_Members'(10, Completed_No));
   end Unmarshall_Component_Data;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register
        (Tag_Code_Sets,
         Create_Empty_Component'Access,
         Fetch_Component'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tagged_components.code_sets",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => PolyORB.Initialization.String_Lists.Empty,
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.GIOP_P.Tagged_Components.Code_Sets;
