------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            E C H O . I M P L                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.4 $
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
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

with Ada.Text_IO;
with CORBA;
with CORBA.NVList;

package body Echo.Impl is

   procedure Invoke
     (Self : access Object;
      R : in CORBA.ServerRequest.Object_Ptr)
   is
      use CORBA;
      use CORBA.NVList;
      use CORBA.ServerRequest;

   begin
      Ada.Text_IO.Put_Line ("In Echo.Impl.Invoke!");
      if CORBA.ServerRequest.Operation (R.all)
        = To_CORBA_String ("echoString") then
         declare
            Args : CORBA.NVList.Ref;

         begin
            CORBA.NVList.Create (Args);

            Add_Item (Args,
                      (Name      => To_CORBA_String ("S"),
                       Argument  => Get_Empty_Any (TypeCode.TC_String),
                       Arg_Modes => ARG_IN));

            Arguments (R, Args);

            declare
               echoString_Arg : CORBA.String :=
                 From_Any (Item (Args, 1).Argument);
            begin
               Set_Result
                 (R, To_Any (EchoString (Self, echoString_Arg)));
            end;
         end;
      else
         raise Program_Error;
      end if;
   end Invoke;

   function EchoString
     (Self : access Object; Mesg : in CORBA.String)
     return CORBA.String is
   begin
      Ada.Text_IO.Put_Line ("Echoing string: « "
                & CORBA.To_Standard_String (Mesg) & " »");
      return Mesg;
   end EchoString;

end Echo.Impl;

