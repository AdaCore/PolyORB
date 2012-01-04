------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . C O R B A _ P . O R B _ I N I T              --
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

with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package body PolyORB.CORBA_P.ORB_Init is

   use PolyORB.Utils;
   use PolyORB.Utils.Strings;

   type ORB_Init_Suffix_Record is record
      Suffix          : String_Ptr;
      ORB_Init_Suffix : ORB_Init_Suffix_Type;
   end record;

   package ORB_Init_Suffix_Record_List is
      new PolyORB.Utils.Chained_Lists (ORB_Init_Suffix_Record);
   use ORB_Init_Suffix_Record_List;

   Callbacks : ORB_Init_Suffix_Record_List.List;

   --------------
   -- Register --
   --------------

   procedure Register
     (Suffix          : String;
      ORB_Init_Suffix : ORB_Init_Suffix_Type)
   is
   begin
      Append (Callbacks,
              ORB_Init_Suffix_Record'(Suffix          => +Suffix,
                                      ORB_Init_Suffix => ORB_Init_Suffix));
   end Register;

   ----------------
   -- Initialize --
   ----------------

   function Initialize (Suffix : String; Value : String) return Boolean is
      It : Iterator := First (Callbacks);

   begin
      while not Last (It) loop
         if Suffix = ORB_Init_Suffix_Record_List.Value (It).Suffix.all then
            return ORB_Init_Suffix_Record_List.Value (It).ORB_Init_Suffix
              (Value);
         end if;

         Next (It);
      end loop;

      return False;
   end Initialize;

   function Initialize (Value : String) return Boolean is
      It : Iterator := First (Callbacks);

   begin
      while not Last (It) loop
         declare
            Suffix : constant String
              := ORB_Init_Suffix_Record_List.Value (It).Suffix.all;

         begin
            if Has_Prefix (Value, Suffix) then
               return ORB_Init_Suffix_Record_List.Value (It).ORB_Init_Suffix
                 (Value (Value'First + Suffix'Length .. Value'Last));
            end if;
         end;

         Next (It);
      end loop;

      return False;
   end Initialize;

end PolyORB.CORBA_P.ORB_Init;
