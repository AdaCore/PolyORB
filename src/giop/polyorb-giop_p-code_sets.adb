------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . G I O P _ P . C O D E _ S E T S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.GIOP_P.Code_Sets.Converters;
with PolyORB.GIOP_P.Code_Sets.Data;
with PolyORB.Parameters;

package body PolyORB.GIOP_P.Code_Sets is

   use PolyORB.Errors;

   function Is_In
     (List : Code_Set_Id_List;
      Item : Code_Set_Id)
     return Boolean;
   --  Return True iff Item is in List

   function Intersection
     (List_A, List_B : Code_Set_Id_List)
     return Code_Set_Id_List;
   --  Return list of Code_Set_Ids that exist in both List_A and List_B

   function Is_Compatible
     (Code_Set_A, Code_Set_B : Code_Set_Id)
     return Boolean;
   --  Return True iff Code_Set_A and Code_Set_B is compatible

   -------------------------------
   -- Conversion_Char_Code_Sets --
   -------------------------------

   function Conversion_Char_Code_Sets return Code_Set_Id_List is
   begin
      return
        Converters.Supported_Char_Conversion_Code_Sets (Native_Char_Code_Set);
   end Conversion_Char_Code_Sets;

   --------------------------------
   -- Conversion_Wchar_Code_Sets --
   --------------------------------

   function Conversion_Wchar_Code_Sets return Code_Set_Id_List is
   begin
      return
        Converters.Supported_Wchar_Conversion_Code_Sets
          (Native_Wchar_Code_Set);
   end Conversion_Wchar_Code_Sets;

   ------------------
   -- Intersection --
   ------------------

   function Intersection
     (List_A, List_B : Code_Set_Id_List)
     return Code_Set_Id_List
   is
      use Code_Set_Id_Lists;

      Result : Code_Set_Id_List;
      Iter   : Code_Set_Id_Lists.Iterator := First (List_A);

   begin
      while not Last (Iter) loop
         if Is_In (List_B, Value (Iter).all) then
            Append (Result, Value (Iter).all);
         end if;

         Next (Iter);
      end loop;

      return Result;
   end Intersection;

   -------------------
   -- Is_Compatible --
   -------------------

   function Is_Compatible
     (Code_Set_A, Code_Set_B : Code_Set_Id)
     return Boolean
   is

      function Find_Info_Index (Code_Set : Code_Set_Id) return Natural;
      --  Return index of Code_Set information in table. Return 0 if
      --  code set is not in information table.

      ---------------------
      -- Find_Info_Index --
      ---------------------

      function Find_Info_Index (Code_Set : Code_Set_Id) return Natural is
      begin
         for J in Data.Info'Range loop
            if Data.Info (J).Code_Set = Code_Set then
               return J;
            end if;
         end loop;

         return 0;
      end Find_Info_Index;

   begin
      pragma Assert (Code_Set_A /= Code_Set_B);

      declare
         Index_A : constant Natural := Find_Info_Index (Code_Set_A);
         Index_B : constant Natural := Find_Info_Index (Code_Set_B);

         Info_A : Code_Set_Info_Record renames Data.Info (Index_A);
         Info_B : Code_Set_Info_Record renames Data.Info (Index_B);

      begin
         if Index_A = 0 or else Index_B = 0 then

            --  No information about code set compatibility
            --  found. Assume code sets are incompatible.

            return False;
         end if;

         --  These checks are based on the knowledg of specific data
         --  representation in generated tables. This reduces the
         --  complexity of the compatibility checks in most cases.

         --  This check assumes that the Character_Sets table is
         --  "packed": if two or more code set have equal character
         --  sets, then this character sets sequence appears only once
         --  in Character_Sets table, thus First and Last indexes of
         --  these code sets are equal.

         if Info_A.First = Info_B.First
           and then Info_A.Last = Info_B.Last
         then
            return True;
         end if;

         --  Code sets are not compatible or data in generated tables
         --  not correctly prepared.

         --  Check that both code sets have only one character set

         if Info_A.Last = Info_A.First
           and then Info_B.Last = Info_B.First
         then
            return
              Data.Character_Sets (Info_A.First)
                = Data.Character_Sets (Info_B.First);
         end if;

         --  If one of code sets have only one character set then code
         --  sets are not compatible.

         if Info_A.Last = Info_A.First
           or else Info_B.Last = Info_B.First
         then
            return False;
         end if;

         --  Default case: for compatibility we require at least two
         --  compatible character sets.

         declare
            Count : Natural := 0;

         begin
            for J in Info_A.First .. Info_A.Last loop
               for K in Info_B.First .. Info_B.Last loop
                  if Data.Character_Sets (J) = Data.Character_Sets (K) then
                     Count := Count + 1;
                     exit;
                  end if;
               end loop;
            end loop;

            return Count >= 2;
         end;
      end;
   end Is_Compatible;

   -----------
   -- Is_In --
   -----------

   function Is_In
     (List : Code_Set_Id_List;
      Item : Code_Set_Id)
      return Boolean
   is
      use Code_Set_Id_Lists;

      Iter : Code_Set_Id_Lists.Iterator := First (List);

   begin
      while not Last (Iter) loop
         if Value (Iter).all = Item then
            return True;
         end if;

         Next (Iter);
      end loop;

      return False;
   end Is_In;

   --------------------------
   -- Native_Char_Code_Set --
   --------------------------

   function Native_Char_Code_Set return Code_Set_Id is
   begin
      return Code_Set_Id
        (PolyORB.Parameters.Get_Conf
         ("giop",
          "giop.native_char_code_set",
          Integer (Ada95_Native_Character_Code_Set)));
   end Native_Char_Code_Set;

   ---------------------------
   -- Native_Wchar_Code_Set --
   ---------------------------

   function Native_Wchar_Code_Set return Code_Set_Id is
   begin
      return Code_Set_Id
        (PolyORB.Parameters.Get_Conf
         ("giop",
          "giop.native_wchar_code_set",
          Integer (Ada95_Native_Wide_Character_Code_Set)));
   end Native_Wchar_Code_Set;

   ------------------------
   -- Negotiate_Code_Set --
   ------------------------

   procedure Negotiate_Code_Set
    (CNCS     : Code_Set_Id;
     CCCS     : Code_Set_Id_List;
     SNCS     : Code_Set_Id;
     SCCS     : Code_Set_Id_List;
     Fallback : Code_Set_Id;
     TCS      : out Code_Set_Id;
     Error    : in out PolyORB.Errors.Error_Container)
   is
   begin
      --  Implementation Note: this algorithm is defined in CORBA3
      --  13.10.2.6 Code Set Negotiation.

      if CNCS = SNCS then

         --  No conversion required

         TCS := CNCS;

      elsif Is_In (CCCS, SNCS) then

         --  Client converts to server's native code set

         TCS := SNCS;

      elsif Is_In (SCCS, CNCS) then

         --  Server converts from client's native code set

         TCS := CNCS;

      else
         declare
            Common : Code_Set_Id_List := Intersection (CCCS, SCCS);

         begin
            if Common /= Code_Set_Id_List (Code_Set_Id_Lists.Empty) then

               --  Client chooses TCS, from intersection, that is
               --  most preferable to server; client converts from
               --  CNCS to TCS and server from TCS to SNCS

               Extract_First (Common, TCS);

            elsif Is_Compatible (CNCS, SNCS) then

               --  Fallback are UTF-8 (for char data) and UTF-16 (for
               --  wchar data)

               TCS := Fallback;

            else
               --  XXX What is the minor code corresponding to this
               --  situation ?

               Throw
                (Error,
                 Codeset_Incompatible_E,
                 System_Exception_Members'
                  (Minor => 255, Completed => Completed_No));
            end if;

            Deallocate (Common);
         end;
      end if;
   end Negotiate_Code_Set;

end PolyORB.GIOP_P.Code_Sets;
