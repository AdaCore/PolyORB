------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    T E S T _ N A M I N G _ C O R B A                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

--  Testing Naming client. Use the CORBA COS Naming API.

--  Designed to interact with others implementation of
--  the CORBA Naming Service

--  $Id$

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with CORBA;
with CORBA.Object;
with CORBA.ORB;

with CosNaming;
with CosNaming.NamingContext;
--  with CosNaming.BindingIterator;

--  with PolyORB.CORBA_P.Naming_Tools;

procedure Test_Naming_CORBA is

   use CORBA.Object;

   use CosNaming;
   use CosNaming.NamingContext;
   --  use CosNaming.BindingIterator;

   --  use PolyORB.CORBA_P.Naming_Tools;

   Root_Context : CosNaming.NamingContext.Ref;

begin

   --
   --  Initialization
   --

   if Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   CORBA.ORB.String_To_Object
     (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)), Root_Context);

   --
   --  Test 1 : bind 1 object, lookup and then destroy
   --

   declare
      Obj_Name : CosNaming.Name;
      Rcvd_Ref : CORBA.Object.Ref;
   begin
      Append (Obj_Name, NameComponent'(Id => To_CORBA_String ("object1"),
                                       Kind => To_CORBA_String ("")));
      bind (Root_Context, Obj_Name,
            CORBA.Object.Ref (Root_Context));

      Rcvd_Ref := resolve (Root_Context, Obj_Name);

      unbind (Root_Context, Obj_Name);
      Rcvd_Ref := resolve (Root_Context, Obj_Name);

   end;


exception
   when E : CORBA.Transient =>
      declare
         Memb : CORBA.System_Exception_Members;
      begin
         CORBA.Get_Members (E, Memb);
         Put ("received exception transient, minor");
         Put (CORBA.Unsigned_Long'Image (Memb.Minor));
         Put (", completion status: ");
         Put_Line (CORBA.Completion_Status'Image (Memb.Completed));
      end;

end Test_Naming_CORBA;
