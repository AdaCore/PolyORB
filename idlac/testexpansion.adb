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
with Idl_Fe.Errors;
with Idl_Fe.Parser;
with Idl_Fe.Display_Tree;

with Ada_Be.Expansion;

procedure Testexpansion is
   Rep : Idl_Fe.Types.Node_Id;
   Display_Tree : Boolean
     := True;
   Exception_Raised : Exception_Occurrence_Access
     := null;

begin
   Put_Line ("Testexpansion : initializing parser");

   loop
      case Getopt ("q") is
         when Ascii.NUL => exit;

         when 'q' =>
            --  Quiet.
            Display_Tree := False;

         when others =>
            raise Program_Error;         -- cannot occur!
      end case;
   end loop;

   Idl_Fe.Parser.Initialize
     (Get_Argument,
      Preprocess => True,
      Keep_Temporary_Files => False);

   Put_Line ("Testexpansion : Parsing");
   Rep := Idl_Fe.Parser.Parse_Specification;
   Put_Line ("Testexpansion : Expanding ");
   begin
      Ada_Be.Expansion.Expand_Repository (Rep);
   exception
      when E : others =>
         Exception_Raised := Save_Occurrence (E);
   end;

   if Display_Tree then
      Put_Line ("Testexpansion : Displaying ");
      Idl_Fe.Display_Tree.Disp_Tree (Rep);
   end if;

   Put_Line ("Testexpansion : Finished ");
   if Idl_Fe.Errors.Is_Error then
      Ada.Text_IO.Put ("there were " &
                       Natural'Image (Idl_Fe.Errors.Error_Number) &
                       " error(s)");
      if Idl_Fe.Errors.Is_Warning then
         Ada.Text_IO.Put_Line (" and " &
                               Natural'Image (Idl_Fe.Errors.Warning_Number) &
                               " warning(s) during parsing.");
      end if;
   else
      if Idl_Fe.Errors.Is_Warning then
         Ada.Text_IO.Put_Line ("there was " &
                               Natural'Image (Idl_Fe.Errors.Warning_Number) &
                               " warning(s) during parsing.");
      else
         Ada.Text_IO.Put_Line ("successfully parsed");
      end if;
   end if;

   if Exception_Raised /= null then
      Put_Line ("Exception raised during expansion:");
      Put_Line (Ada.Exceptions.Exception_Information (Exception_Raised.all));
   end if;

end Testexpansion;
