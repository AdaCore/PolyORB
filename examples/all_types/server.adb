------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $
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

with all_types.Impl;

with CORBA;
with CORBA.Object;

with Broca.Server_Tools; use Broca.Server_Tools;

with Naming_Tools; use Naming_Tools;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;

procedure Server is

   Ref   : CORBA.Object.Ref;
   Guard : Server_Guard;

begin
   Ada.Text_IO.Put_Line ("Server begins here");
   Initiate_Servant (new all_types.Impl.Object, Ref);
   if Argument_Count = 1 then
      if Argument (1) = "-s" then
         Register (Guard, "all_types", Ref, True);
      else
         Register (Guard, Argument (1), Ref, True);
      end if;
   end if;
   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
      "'");
   Initiate_Server (Start_New_Task => False);
end Server;
