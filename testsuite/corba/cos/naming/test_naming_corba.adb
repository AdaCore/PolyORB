------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    T E S T _ N A M I N G _ C O R B A                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2023, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

--  Testing Naming client. Use the CORBA COS Naming API.

--  Designed to interact with others implementation of
--  the CORBA Naming Service

with Ada.Command_Line;
with Ada.Text_IO;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with CORBA.Object;
with CORBA.ORB;

with CosNaming.NamingContext;
with CosNaming.NamingContextExt;

with PolyORB.Utils.Report;

procedure Test_Naming_CORBA is

   use Ada.Command_Line;
   use Ada.Text_IO;

   use CORBA.Object;

   use CosNaming;
   use CosNaming.NamingContext;
   use CosNaming.NamingContextExt;
   use PolyORB.Utils.Report;

begin
   New_Test ("CORBA COS Naming");

   --  Initialization

   CORBA.ORB.Initialize ("ORB");

   if Argument_Count < 1 then
      Put_Line ("usage : test_naming_corba <IOR_string_from_server>");
      return;
   end if;

   --
   --  Test 1 : bind 1 object, lookup and then destroy
   --

   declare
      Obj_Name : CosNaming.Name;
      Rcvd_Ref : CORBA.Object.Ref;
      pragma Unreferenced (Rcvd_Ref);
      Root_Context : CosNaming.NamingContext.Ref;

   begin
      New_Test ("Bind 1 object, lookup and then destroy");

      CORBA.ORB.String_To_Object
        (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)), Root_Context);

      Output ("Retrieve Root_Context", True);

      Append (Obj_Name, NameComponent'(id => To_CORBA_String ("object1"),
                                       kind => To_CORBA_String ("")));

      bind (Root_Context, Obj_Name, CORBA.Object.Ref (Root_Context));

      Output ("Bind Object", True);

      begin
         bind (Root_Context, Obj_Name, CORBA.Object.Ref (Root_Context));

         Output ("Bind Object (raise Already Bound)", False);
      exception
         when CosNaming.NamingContext.AlreadyBound =>
            Output ("Bind Object (raise Already Bound)", True);

         when others =>
            Output ("Bind Object (raise Already Bound)", False);
      end;

      Rcvd_Ref := resolve (Root_Context, Obj_Name);

      Output ("Resolve Object", True);

      rebind (Root_Context, Obj_Name, CORBA.Object.Ref (Root_Context));

      Output ("Rebind Object", True);

      unbind (Root_Context, Obj_Name);

      Output ("Unbind Object", True);

      begin
         Rcvd_Ref := resolve (Root_Context, Obj_Name);
         Output ("Resolve unbound reference raise NotFound", False);

      exception
         when CosNaming.NamingContext.NotFound =>
            Output ("Resolve unbound reference raise NotFound", True);
      end;
   end;

   --
   --  Test 2 : bind 1 object, lookup and then destroy
   --

   declare
      Obj_Name : CosNaming.Name;
      Rcvd_Ref : CORBA.Object.Ref;
      pragma Unreferenced (Rcvd_Ref);
      Root_Context : CosNaming.NamingContextExt.Ref;

   begin
      New_Test ("NamingContextExt tests");

      CORBA.ORB.String_To_Object
        (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)), Root_Context);

      Output ("Retrieve Root_Context", True);

      Append (Obj_Name, NameComponent'(id => To_CORBA_String ("object1"),
                                       kind => To_CORBA_String ("id1")));

      Append (Obj_Name, NameComponent'(id => To_CORBA_String ("object2"),
                                       kind => To_CORBA_String ("")));

      Append (Obj_Name, NameComponent'(id => To_CORBA_String ("object3"),
                                       kind => To_CORBA_String ("id3")));

      declare
         The_String : constant String
           := To_String (CosNaming.NamingContextExt.to_string
                         (Root_Context, Obj_Name));
      begin
         Output ("NamingContextExt::To_String",
                 The_String = "object1.id1/object2/object3.id3");
      end;

      declare
         Obj_Name2 : constant CosNaming.Name
           := to_name (Root_Context,
                       CosNaming.NamingContextExt.to_string
                       (Root_Context, Obj_Name));
      begin
         Output ("NamingContextExt::To_Name", Obj_Name2 = Obj_Name);
      end;

      declare
         The_String : constant String
           := CosNaming.NamingContextExt.To_String
              (CosNaming.NamingContextExt.to_url
               (Root_Context,
                CosNaming.NamingContextExt.To_CORBA_String
                (":myhost.mydomain.com"),
                CosNaming.NamingContextExt.To_CORBA_String ("ppp/ppp")));
      begin
         Output ("NamingContextExt::To_Url",
                 The_String = "%3amyhost.mydomain.com/ppp%2fppp");
      end;
   end;

   End_Report;

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
         End_Report;
      end;
end Test_Naming_CORBA;
