------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . M O M A _ P . T O O L S                  --
--                                                                          --
--                                 S p e c                                  --
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

--  $Id$

--  XXX a higher level Initialize_OA function, built in concordance
--  with other application personalities, is required for nodes mixing
--  different application personalities .....

with PolyORB.Log;
with PolyORB.Minimal_Servant;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Objects;
with PolyORB.Servants;
with PolyORB.ORB;
with PolyORB.Setup;

package body PolyORB.MOMA_P.Tools is

   use PolyORB.Log;
   use PolyORB.Minimal_Servant;
   use PolyORB.Setup;
   use PolyORB.Objects;
   use PolyORB.ORB;
   use PolyORB.Servants;

   package L is new PolyORB.Log.Facility_Log ("polyorb.moma_p.tools");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Is_OA_Initialized : Boolean := False;

   procedure Initialize_OA;

   -------------------
   -- Initialize_OA --
   -------------------

   procedure Initialize_OA
   is
      Obj_Adapter : PolyORB.Obj_Adapters.Obj_Adapter_Access;
   begin
      if not Is_OA_Initialized then

         pragma Debug (O ("Creating object adapter..."));
         Obj_Adapter := new PolyORB.Obj_Adapters.Simple.Simple_Obj_Adapter;
         PolyORB.Obj_Adapters.Create (Obj_Adapter);
         --  Create object adapter

         Set_Object_Adapter (The_ORB, Obj_Adapter);
         --  Link object adapter with ORB.

         Is_OA_Initialized := True;

      end if;
   end Initialize_OA;

   ----------------------
   -- Initiate_Servant --
   ----------------------

   procedure Initiate_Servant
     (Obj     : access PolyORB.Minimal_Servant.Servant'Class;
      If_Desc : in     PolyORB.Obj_Adapters.Simple.Interface_Description;
      Ref     : out    PolyORB.References.Ref)
   is
      Servant : constant PolyORB.Servants.Servant_Access
        := To_PolyORB_Servant (Obj);

      Obj_Adapter : PolyORB.Obj_Adapters.Obj_Adapter_Access;
   begin
      Initialize_OA;
      Obj_Adapter := PolyORB.ORB.Object_Adapter (The_ORB);

      declare
         Servant_Id : constant Object_Id_Access
           := new Object_Id'(PolyORB.Obj_Adapters.Export (Obj_Adapter,
                                                          Servant));
         --  Register it with the SOA.

      begin
         PolyORB.Obj_Adapters.Simple.Set_Interface_Description
           (PolyORB.Obj_Adapters.Simple.Simple_Obj_Adapter
            (Obj_Adapter.all), Servant_Id, If_Desc);
         --  Set object description.

         Create_Reference (The_ORB, Servant_Id, "MOMA", Ref);
      end;
   end Initiate_Servant;

   ----------------
   -- Run_Server --
   ----------------

   procedure Run_Server is
   begin
      PolyORB.ORB.Run (PolyORB.Setup.The_ORB, May_Poll => True);
   end Run_Server;

end PolyORB.MOMA_P.Tools;
