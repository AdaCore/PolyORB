------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            E C H O . I M P L                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id: //droopi/main/src/echo-impl.adb#10 $

with Ada.Text_IO;

with CORBA.ORB;
--  with CORBA.ExceptionList;
--  with CORBA.ContextList;
with CORBA.NVList;
--  with CORBA.Context;
--  with CORBA.Object;
with PolyORB.CORBA_P.Exceptions;
with CORBA; use CORBA;
pragma Elaborate_All (CORBA);

package body Echo.Impl is


   function echoString
     (Self : access Object;
      Mesg : in CORBA.String)
     return CORBA.String
   is
      Result : CORBA.String;
   begin

      --  Insert implementation of echoString
      Result := Mesg;
      Ada.Text_IO.Put_Line (CORBA.To_Standard_String (Mesg));
      return Result;
   end echoString;


   procedure Invoke
     (Self : access Object;
      Request : in CORBA.ServerRequest.Object_ptr)
   is
      Operation : Standard.String
        := CORBA.To_Standard_String (CORBA.ServerRequest.Operation (Request.all));
   begin
      if Operation = "echoString" then
         declare
            Mesg            : CORBA.String;
            Arg_Name_Ü_Mesg : CORBA.Identifier := To_CORBA_String ("Mesg");
            Argument_Ü_Mesg : CORBA.Any := CORBA.To_Any (Mesg);

            Result_Ü        : CORBA.String;
            Argument_Ü_Result_Ü : CORBA.Any ;
            --  Ctx_Ü           : CORBA.Context.Ref := CORBA.Context.Nil_Ref;
            Arg_List_Ü      : CORBA.NVList.Ref;
         begin
            --  Create argument list

            CORBA.ORB.Create_List (0, Arg_List_Ü);
            CORBA.NVList.Add_Item (Arg_List_Ü,
                                   Arg_Name_Ü_Mesg,
                                   Argument_Ü_Mesg,
                                   CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               Mesg := From_Any (Argument_Ü_Mesg);
               Result_Ü := echoString (Self, Mesg);
            end;

            -- Set Result

            Argument_Ü_Result_Ü := CORBA.To_Any (Result_Ü);
            CORBA.ServerRequest.Set_Result (Request, Argument_Ü_Result_Ü);
            return;
         end;
      end if;
      PolyORB.CORBA_P.Exceptions.Raise_Bad_Operation;
   end Invoke;

   function Primary_Interface (Self : access Object; -- ....
                               POA_Ptr : PortableServer.POA.Ref) return String is
   begin
      return "IDL:Echo:1.0";
   end Primary_Interface;

end Echo.Impl;
