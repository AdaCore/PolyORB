------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        T E S T E X P A N S I O N                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;

with Idl_Fe.Types;
with Idl_Fe.Parser;
with Idl_Fe.Display_Tree;
with Ada_Be.Expansion;

with Errors;

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
         when Ascii.NUL => exit;

         when 'q' =>
            --  Quiet.
            Display_Tree := False;

         when 'e' =>
            Expand := True;

         when others =>
            raise Program_Error;         -- cannot occur!
      end case;
   end loop;

   Idl_Fe.Parser.Initialize
     (Get_Argument,
      Preprocess => True,
      Keep_Temporary_Files => False);

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
   if Errors.Is_Error then
      Put (Current_Error, "There were " &
           Natural'Image (Errors.Error_Number) &
           " error(s)");
      if Errors.Is_Warning then
         Put_Line (Current_Error, " and " &
                   Natural'Image (Errors.Warning_Number) &
                   " warning(s) during parsing.");
      end if;
   else
      if Errors.Is_Warning then
         Put_Line (Current_Error, "there was " &
                   Natural'Image (Errors.Warning_Number) &
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

end Testparser;
