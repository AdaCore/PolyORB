------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S E T U P . T E S T _ S O A                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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

--  Set up a test ORB.

--  $Id$

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with PolyORB.Objects;
with PolyORB.ORB;

with PolyORB.Any;
with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;
with PolyORB.Components;
with PolyORB.Exceptions;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Objects;
with PolyORB.Servants;
with PolyORB.ORB.Interface;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Requests;
with PolyORB.Smart_Pointers;
with PolyORB.Types;

--  Our application object.
with PolyORB.Test_Object_SOA;

pragma Elaborate_All (PolyORB.ORB);

package body PolyORB.Setup.Test_SOA is

   use PolyORB.Exceptions;
   use PolyORB.Objects;
   use PolyORB.ORB;

   Obj_Adapter : Obj_Adapters.Obj_Adapter_Access;
   My_Servant : Servants.Servant_Access;

   procedure Initialize_Test_Object
   is
      My_Id  : Object_Id_Access;
      My_Ref : PolyORB.References.Ref;
      Error  : Error_Container;

   begin
      ----------------------------------
      -- Create simple object adapter --
      ----------------------------------

      Put ("Creating object adapter...");
      Obj_Adapter := new Obj_Adapters.Simple.Simple_Obj_Adapter;
      Obj_Adapters.Create (Obj_Adapter);
      --  Create object adapter

      Set_Object_Adapter (The_ORB, Obj_Adapter);
      --  Link object adapter with ORB.

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
      --  Set
      --  (My_Ref, PolyORB.References.Entity_Of (My_Ref));
      --  Obtain object reference.

      Put_Line ("Registered object: " & Image (My_Id.all));
      Put_Line ("Reference is     : " & References.Image (My_Ref));
      begin
         Put_Line ("IOR is           : "
                   & PolyORB.Types.To_Standard_String
                   (PolyORB.References.IOR.Object_To_String (My_Ref)));
      exception
         when E : others =>
            Put_Line ("Warning: Object_To_String raised:");
            Put_Line (Ada.Exceptions.Exception_Information (E));
      end;
   end Initialize_Test_Object;

   procedure Run_Test is
   begin
      --  Check if we simply run the ORB to accept remote acccess
      --  or if we run a local request to the ORB
      if Ada.Command_Line.Argument_Count = 1
        and then Ada.Command_Line.Argument (1) = "local" then

         ---------------------------------------
         -- Create a local request to the ORB --
         ---------------------------------------

         declare
            use PolyORB.Any;
            use PolyORB.Any.NVList;
            use PolyORB.Components;
            use PolyORB.ORB.Interface;
            use PolyORB.Requests;
            use PolyORB.Types;

            Req : Request_Access;
            Args : Any.NVList.Ref;
            Result : Any.NamedValue;

            procedure Create_WaitAndEchoString_Request
              (Arg1 : String;
               Arg2 : Integer);


            procedure Create_WaitAndEchoString_Request
              (Arg1 : String;
               Arg2 : Integer)
            is
            begin
               Create (Args);
               Add_Item
                 (Args,
                  To_PolyORB_String ("waitAndEchoString"),
                  To_Any (To_PolyORB_String (Arg1)),
                  ARG_IN);
               Add_Item
                 (Args,
                  To_PolyORB_String ("waitAndEchoString"),
                  To_Any (Long (Arg2)),
                  ARG_IN);

               Put ("Creating servant request...  ");
               Create_Request
                 (My_Ref,
                  "waitAndEchoString",
                  Args,
                  Result,
                  PolyORB.Any.ExceptionList.Nil_Ref,
                  Req);
               Put_Line ("Done...");

               Emit_No_Reply
                 (Component_Access (The_ORB),
                  Queue_Request'(Request   => Req,
                                 Requestor => null));
               --  Requesting_Task => null));
            end Create_WaitAndEchoString_Request;
         begin
            Create_WaitAndEchoString_Request
              ("request number 1 : wait 3 seconds", 3);
            Create_WaitAndEchoString_Request
              ("request number 2 : wait 2 seconds", 2);
            Create_WaitAndEchoString_Request
              ("request number 3 : wait 2 seconds", 2);
            Create_WaitAndEchoString_Request
              ("request number 4 : wait 2 seconds", 2);

            Run (The_ORB,
                 (Condition =>
                    Req.Completed'Access,
                  Task_Info => Req.Requesting_Task'Access),
                 May_Poll => True);
            --  Execute the ORB.
         end;

      else

         Run (The_ORB, May_Poll => True);
         --  Execute the ORB.

      end if;
   end Run_Test;

end PolyORB.Setup.Test_SOA;


