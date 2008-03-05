------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         E C H O _ D Y N I M P L                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2008, Free Software Foundation, Inc.          --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.NVList;
with CORBA.ORB;

package body Echo_DynImpl is

   ------------
   -- Invoke --
   ------------

   procedure Invoke
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
            Arg_Name_Mesg : CORBA.Identifier := CORBA.To_CORBA_String ("Mesg");
            Argument_Mesg : CORBA.Any := CORBA.To_Any (Mesg);

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
