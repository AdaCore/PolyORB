------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . C O R B A _ P . I R _ T O O L S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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
