------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           T E S T P A R S E R                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;

with Idl_Fe.Types;
with Idl_Fe.Parser;
with Idl_Fe.Display_Tree;
with Ada_Be.Expansion;

with Idlac_Errors;

procedure Testparser is
   Rep : Idl_Fe.Types.Node_Id;
   Display_Tree : Boolean
     := True;
   Expand : Boolean := False;
   Exception_Raised : Exception_Occurrence_Access
     := null;

begin
   Put_Line (Current_Error, "Testparser : initializing parser");

   loop
      case Getopt ("q e") is
         when ASCII.NUL => exit;

         when 'q' =>
            --  Quiet.
            Display_Tree := False;

         when 'e' =>
            Expand := True;

         when others =>
            raise Program_Error;         -- cannot occur!
      end case;
   end loop;

   Idl_Fe.Parser.Initialize (Get_Argument);

   Put_Line (Current_Error, "Testparser : Parsing");
   Rep := Idl_Fe.Parser.Parse_Specification;

   if Expand then
      Put_Line (Current_Error, "Testparser : Expanding ");
      begin
         Ada_Be.Expansion.Expand_Repository (Rep);
      exception
         when E : others =>
            Exception_Raised := Save_Occurrence (E);
      end;
   end if;

   if Display_Tree then
      Put_Line (Current_Error, "Testparser : Displaying ");
      Idl_Fe.Display_Tree.Disp_Tree (Rep);
   end if;

   Put_Line (Current_Error, "Testparser : Finished ");
   if Idlac_Errors.Is_Error then
      Put (Current_Error, "There were " &
           Natural'Image (Idlac_Errors.Error_Number) &
           " error(s)");
      if Idlac_Errors.Is_Warning then
         Put_Line (Current_Error, " and " &
                   Natural'Image (Idlac_Errors.Warning_Number) &
                   " warning(s) during parsing.");
      end if;
   else
      if Idlac_Errors.Is_Warning then
         Put_Line (Current_Error, "there was " &
                   Natural'Image (Idlac_Errors.Warning_Number) &
                   " warning(s) during parsing.");
      else
         Put_Line (Current_Error, "successfully parsed");
      end if;
   end if;

   if Exception_Raised /= null then
      Put_Line
        (Current_Error, "Exception raised during expansion:");
      Put_Line
        (Current_Error,
         Ada.Exceptions.Exception_Information
         (Exception_Raised.all));
   end if;

   Idl_Fe.Parser.Finalize;
end Testparser;
