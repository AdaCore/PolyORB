------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S E T U P . T E S T _ S O A                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  Set up a test ORB.

with Ada.Exceptions;
with Ada.Text_IO;

with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Objects;
with PolyORB.ORB.Iface;
with PolyORB.References.IOR;
with PolyORB.Requests;
with PolyORB.Servants;
with PolyORB.Types;

with PolyORB.Utils.Report;

--  Our application object.
with PolyORB.Test_Object_SOA;

package body PolyORB.Setup.Test_SOA is

   use Ada.Text_IO;

   use PolyORB.Errors;
   use PolyORB.Objects;
   use PolyORB.ORB;
   use PolyORB.Utils.Report;

   Obj_Adapter : Obj_Adapters.Obj_Adapter_Access;
   My_Servant : Servants.Servant_Access;

   ----------------------------
   -- Initialize_Test_Object --
   ----------------------------

   procedure Initialize_Test_Object is
      My_Id  : Object_Id_Access;
      Error  : Error_Container;

   begin
      ----------------------------------
      -- Create simple object adapter --
      ----------------------------------

      Obj_Adapter := new Obj_Adapters.Simple.Simple_Obj_Adapter;
      Obj_Adapters.Create (Obj_Adapter);
      --  Create object adapter

      Set_Object_Adapter (The_ORB, Obj_Adapter);
      --  Link object adapter with ORB.

      Output ("Created object adapter", True);

      My_Servant := new Test_Object_SOA.My_Object;
      --  Create application server object.

      Obj_Adapters.Export (Obj_Adapter,
                           My_Servant,
                           null,
                           My_Id,
                           Error);
      --  Register it with the SOA.

      if Found (Error) then
         raise Program_Error;
      end if;

      Obj_Adapters.Simple.Set_Interface_Description
        (Obj_Adapters.Simple.Simple_Obj_Adapter (Obj_Adapter.all),
         My_Id, Test_Object_SOA.If_Desc);
      --  Set object description.

      Create_Reference (The_ORB, My_Id, "IDL:Echo:1.0", My_Ref);

      Output ("Registered object: " & Image (My_Id.all), True);
      Put_Line ("Reference is     : " & References.Image (My_Ref));
      begin
         Put_Line ("IOR is           : "
                   & PolyORB.References.IOR.Object_To_String (My_Ref));
      exception
         when E : others =>
            Put_Line ("Warning: Object_To_String raised:");
            Put_Line (Ada.Exceptions.Exception_Information (E));
      end;
   end Initialize_Test_Object;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test is
   begin
      ---------------------------------------
      -- Create a local request to the ORB --
      ---------------------------------------

      declare
         use PolyORB.Any;
         use PolyORB.Any.NVList;
         use PolyORB.Components;
         use PolyORB.ORB.Iface;
         use PolyORB.Requests;
         use PolyORB.Types;

         Req : Request_Access;
         Args : Any.NVList.Ref;
         Result : Any.NamedValue;

         procedure Create_echoString_Request (Arg1 : String);

         procedure Create_echoString_Request (Arg1 : String) is
         begin
            Create (Args);
            Add_Item
              (Args,
               To_PolyORB_String ("echoString"),
               To_Any (Arg1),
               ARG_IN);

            Create_Request
              (My_Ref,
               "echoString",
               Args,
               Result,
               PolyORB.Any.ExceptionList.Nil_Ref,
               Req);
            Output ("Created servant request", True);

            Emit_No_Reply
              (Component_Access (The_ORB),
               Queue_Request'(Request   => Req,
                              Requestor => null));
         end Create_echoString_Request;
      begin
         for J in 1 .. 4 loop
            Create_echoString_Request ("request number" & J'Img);
         end loop;

         --  Execute the ORB main loop

         Run (The_ORB, Request  => Req, May_Exit => True);

         End_Report;
      end;
   end Run_Test;

end PolyORB.Setup.Test_SOA;
