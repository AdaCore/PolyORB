------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            M O M A . C O N F I G U R A T I O N . S E R V E R             --
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

--  $Id$

with MOMA.Provider.Message_Pool;
with MOMA.Provider.Routers;

with PolyORB.Log;
with PolyORB.Minimal_Servant.Tools;

package body MOMA.Configuration.Server is

   use PolyORB.Configuration;
   use PolyORB.Minimal_Servant.Tools;
   use PolyORB.Log;
   use PolyORB.References;

   use MOMA.Types;

   package L is new PolyORB.Log.Facility_Log ("moma.configuration.server");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   -------------------------
   -- Create_Message_Pool --
   -------------------------

   procedure Create_Message_Pool (Pool : MOMA.Types.Message_Pool;
                                  Ref  : out PolyORB.References.Ref)
   is
      MOMA_Obj : constant MOMA.Provider.Message_Pool.Object_Acc
       := new MOMA.Provider.Message_Pool.Object;

   begin
      pragma Debug (O ("Creating Message Pool "
                       & To_Standard_String (Get_Name (Pool))));
      Initiate_Servant (MOMA_Obj,
                        MOMA.Provider.Message_Pool.If_Desc,
                        MOMA_Type_Id,
                        Ref);
      MOMA.Provider.Message_Pool.Initialize (MOMA_Obj, Pool);
   end Create_Message_Pool;

   -------------------
   -- Create_Router --
   -------------------

   procedure Create_Router (Id         : MOMA.Types.String;
                            Ref        : out PolyORB.References.Ref;
                            Router_Ref : PolyORB.References.Ref :=
                                            PolyORB.References.Nil_Ref)
   is
      Router : constant MOMA.Provider.Routers.Router_Acc
       := new MOMA.Provider.Routers.Router;
   begin
      pragma Debug (O ("Creating Router"));
      MOMA.Provider.Routers.Set_Id (Router.all, Id);
      Initiate_Servant (Router,
                        MOMA.Provider.Routers.If_Desc,
                        MOMA_Type_Id,
                        Ref);
      MOMA.Provider.Routers.Set_Self_Ref (Router.all, Ref);
      MOMA.Provider.Routers.Initialize (Router, Router_Ref);
   end Create_Router;

end MOMA.Configuration.Server;
