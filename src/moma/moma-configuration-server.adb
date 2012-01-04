------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            M O M A . C O N F I G U R A T I O N . S E R V E R             --
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

with MOMA.Runtime;

with PolyORB.MOMA_P.Provider.Message_Pool;
with PolyORB.MOMA_P.Provider.Routers;

with PolyORB.Errors;
with PolyORB.Log;
with PolyORB.Minimal_Servant.Tools;
with PolyORB.MOMA_P.Exceptions;
with PolyORB.Types;

package body MOMA.Configuration.Server is

   use PolyORB.Errors;
   use PolyORB.Log;
   use PolyORB.Minimal_Servant.Tools;

   use MOMA.Types;

   package L is new PolyORB.Log.Facility_Log ("moma.configuration.server");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   -------------------------
   -- Create_Message_Pool --
   -------------------------

   procedure Create_Message_Pool
     (Pool :     MOMA.Types.Message_Pool;
      Ref  : out MOMA.Types.Ref)
   is
      MOMA_Obj : constant PolyORB.MOMA_P.Provider.Message_Pool.Object_Acc
        := new PolyORB.MOMA_P.Provider.Message_Pool.Object;

      Error : Error_Container;

   begin
      pragma Debug (C, O ("Creating Message Pool "
                       & To_Standard_String (Get_Name (Pool))));

      Initiate_Servant
        (MOMA_Obj,
         MOMA.Runtime.MOMA_OA,
         PolyORB.Types.String (MOMA_Type_Id),
         Ref,
         Error);

      if Found (Error) then
         PolyORB.MOMA_P.Exceptions.Raise_From_Error (Error);
      end if;

      PolyORB.MOMA_P.Provider.Message_Pool.Initialize (MOMA_Obj, Pool);
   end Create_Message_Pool;

   -------------------
   -- Create_Router --
   -------------------

   procedure Create_Router
     (Id         :     MOMA.Types.String;
      Ref        : out MOMA.Types.Ref;
      Router_Ref :     MOMA.Types.Ref := MOMA.Types.Nil_Ref)
   is
      Router : constant PolyORB.MOMA_P.Provider.Routers.Router_Acc
       := new PolyORB.MOMA_P.Provider.Routers.Router;

      Error : Error_Container;

   begin
      pragma Debug (C, O ("Creating Router"));

      Initiate_Servant
        (Router,
         MOMA.Runtime.MOMA_OA,
         PolyORB.Types.String (MOMA_Type_Id),
         Ref,
         Error);

      if Found (Error) then
         PolyORB.MOMA_P.Exceptions.Raise_From_Error (Error);
      end if;

      PolyORB.MOMA_P.Provider.Routers.Set_Id (Router.all, Id);
      PolyORB.MOMA_P.Provider.Routers.Set_Self_Ref (Router.all, Ref);
      PolyORB.MOMA_P.Provider.Routers.Initialize (Router, Router_Ref);
   end Create_Router;

end MOMA.Configuration.Server;
