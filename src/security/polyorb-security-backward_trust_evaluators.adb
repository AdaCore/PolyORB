------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.SECURITY.BACKWARD_TRUST_EVALUATORS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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
