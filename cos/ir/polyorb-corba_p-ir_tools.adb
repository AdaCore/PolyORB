------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . C O R B A _ P . I R _ T O O L S              --
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

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

with CORBA.Object;
with CORBA.ORB;
with CORBA.Repository_Root.Repository.Helper;
with PolyORB.CORBA_P.IR_Hooks;

package body PolyORB.CORBA_P.IR_Tools is

   function Get_Interface_Definition
     (Id : CORBA.RepositoryId) return CORBA.Object.Ref'Class;
   --  Actual implementation of the Interface Repository hook routine
   --  to be used when the Interface Repository is available.

   procedure Initialize;

   Repo_Root_Ref : CORBA.Repository_Root.Repository.Ref;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PolyORB.CORBA_P.IR_Hooks.Get_Interface_Definition :=
        Get_Interface_Definition'Access;
   end Initialize;

   ------------------------------
   -- Get_Interface_Definition --
   ------------------------------

   function Get_Interface_Definition
     (Id : CORBA.RepositoryId) return CORBA.Object.Ref'Class is
   begin
      return CORBA.Repository_Root.Repository.lookup_id (Get_IR_Root, Id);
   end Get_Interface_Definition;

   -----------------
   -- Get_IR_Root --
   -----------------

   function Get_IR_Root return CORBA.Repository_Root.Repository.Ref is
   begin
      if CORBA.Repository_Root.Repository.Is_Nil (Repo_Root_Ref) then
         Repo_Root_Ref :=
           CORBA.Repository_Root.Repository.Helper.To_Ref
           (CORBA.ORB.Resolve_Initial_References
            (CORBA.ORB.To_CORBA_String ("InterfaceRepository")));
      end if;

      return Repo_Root_Ref;
   end Get_IR_Root;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name       => +"corba_p.ir_tools",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.CORBA_P.IR_Tools;
