------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           T E S T P A R S E R                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
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
