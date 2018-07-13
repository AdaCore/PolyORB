------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         E C H O _ D Y N I M P L                          --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with CORBA.NVList;
with CORBA.ORB;

package body Echo_DynImpl is

   ------------
   -- Invoke --
   ------------

   overriding procedure Invoke
     (Self    : access Object;
      Request : CORBA.ServerRequest.Object_Ptr)
   is
      pragma Unreferenced (Self);

      Operation : constant Standard.String
        := CORBA.To_Standard_String
        (CORBA.ServerRequest.Operation (Request.all));

   begin
      if Operation = "echoString" then
         declare
            Mesg          : CORBA.String;
            Arg_Name_Mesg : constant CORBA.Identifier :=
                              CORBA.To_CORBA_String ("Mesg");
            Argument_Mesg : constant CORBA.Any := CORBA.To_Any (Mesg);

            Result          : CORBA.String;
            Argument_Result : CORBA.Any;
            Arg_List        : CORBA.NVList.Ref;
         begin
            --  Create argument list

            CORBA.ORB.Create_List (0, Arg_List);
            CORBA.NVList.Add_Item (Arg_List,
                                   Arg_Name_Mesg,
                                   Argument_Mesg,
                                   CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List);

            begin
               --  Convert arguments from their Any

               Mesg := CORBA.From_Any (Argument_Mesg);

               --  Actual implementation of the echoString function:
               --  simply return the argument

               Result := Mesg;
            end;

            --  Set Result

            Argument_Result := CORBA.To_Any (Result);
            CORBA.ServerRequest.Set_Result (Request, Argument_Result);
            return;
         end;
      end if;

      CORBA.Raise_Bad_Operation (CORBA.Default_Sys_Member);
   end Invoke;

end Echo_DynImpl;
