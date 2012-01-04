------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.SECURITY.BACKWARD_TRUST_EVALUATORS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;

package body PolyORB.Security.Backward_Trust_Evaluators is

   -------------------------------------
   -- Create_Backward_Trust_Evaluator --
   -------------------------------------

   function Create_Backward_Trust_Evaluator
     (File : String) return Backward_Trust_Evaluator_Access
   is
      Result : constant Backward_Trust_Evaluator_Access
        := new Backward_Trust_Evaluator;

   begin
      Result.File_Name := PolyORB.Types.To_PolyORB_String (File);

      return Result;
   end Create_Backward_Trust_Evaluator;

   --------------------
   -- Evaluate_Trust --
   --------------------

   procedure Evaluate_Trust
     (Evaluator         : access Backward_Trust_Evaluator;
      Client_Identity   :        PolyORB.Security.Identities.Identity_Access;
      Asserted_Identity :        PolyORB.Security.Identities.Identity_Access;
      Trusted           :    out Boolean)
   is
      use Ada.Text_IO;
      use PolyORB.Security.Identities;

      File             : File_Type;
      Buffer           : String (1 .. 1024);
      Last             : Natural;
      Section_Find     : Boolean := False;

   begin
      Trusted := False;

      Open
        (File,
         In_File,
         PolyORB.Types.To_Standard_String (Evaluator.File_Name));

      while not End_Of_File (File) loop
         Get_Line (File, Buffer, Last);

         if Last /= 0 then
            if Buffer (1) = '[' then
               Section_Find :=
                 Buffer (1 .. Last) = Get_Printable_Name (Client_Identity);

            elsif Section_Find
              and then Buffer (1 .. Last)
              = ' ' & Get_Printable_Name (Asserted_Identity)
            then
               Trusted := True;

               exit;
            end if;
         end if;
      end loop;

      Close (File);
   end Evaluate_Trust;

end PolyORB.Security.Backward_Trust_Evaluators;
