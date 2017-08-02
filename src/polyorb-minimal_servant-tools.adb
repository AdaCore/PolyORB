------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . M I N I M A L _ S E R V A N T . T O O L S         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2017, Free Software Foundation, Inc.          --
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

with PolyORB.Objects;
with PolyORB.ORB;
with PolyORB.Servants;
with PolyORB.Setup;

package body PolyORB.Minimal_Servant.Tools is

   use PolyORB.Objects;
   use PolyORB.Setup;

   ----------------------
   -- Initiate_Servant --
   ----------------------

   procedure Initiate_Servant
     (Obj     : access PolyORB.Minimal_Servant.Servant'Class;
      Type_Id : PolyORB.Types.String;
      Ref     :    out PolyORB.References.Ref;
      Error   : in out PolyORB.Errors.Error_Container)
   is
   begin
      Initiate_Servant
        (Obj,
         PolyORB.ORB.Object_Adapter (The_ORB),
         Type_Id,
         Ref,
         Error);
   end Initiate_Servant;

   procedure Initiate_Servant
     (Obj          : access PolyORB.Minimal_Servant.Servant'Class;
      Obj_Adapter  : PolyORB.Obj_Adapters.Obj_Adapter_Access;
      Type_Id      : PolyORB.Types.String;
      Ref          :    out PolyORB.References.Ref;
      Error        : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Errors;

      Servant : constant PolyORB.Servants.Servant_Access
        := To_PolyORB_Servant (Obj);
      Servant_Id : Object_Id_Access;

   begin
      PolyORB.Obj_Adapters.Export
        (Obj_Adapter.all'Access, Servant, null, Servant_Id, Error);

      if Found (Error) then
         return;
      end if;

      --  Register object

      PolyORB.ORB.Create_Reference
        (The_ORB,
         Servant_Id,
         PolyORB.Types.To_Standard_String (Type_Id),
         Ref);

      Free (Servant_Id);
   end Initiate_Servant;

   ----------------
   -- Run_Server --
   ----------------

   procedure Run_Server is
   begin
      PolyORB.ORB.Run (PolyORB.Setup.The_ORB, May_Exit => False);
   end Run_Server;

end PolyORB.Minimal_Servant.Tools;
