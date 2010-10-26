------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . I N I T I A L I Z A T I O N                --
--                                                                          --
--                                 S p e c                                  --
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

--  Automatic initialization of PolyORB subsystems.

with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;

package PolyORB.Initialization is

   pragma Preelaborate;

   package String_Lists renames PolyORB.Utils.Strings.Lists;

   type Initializer is access procedure;
   type Finalizer is access
     procedure (Wait_For_Completion : Boolean);

   type Module_Info is record
      Name : Utils.Strings.String_Ptr;
      --  The unique name of this module.

      Provides  : String_Lists.List;
      --  A list of 'virtual' modules provided by this one.
      --  Several different implementations of the same service may exist: they
      --  shall have different Names, but will list the same common name in
      --  their Provides list. If an exclamation mark (!) is appended to a
      --  provided name, this is equivalent to also listing that module in
      --  the Conflicts list (preventing any other module from providing the
      --  same virtual module).

      Depends   : String_Lists.List;
      --  The list of modules this one depends upon. If a question mark is
      --  appended to a name in Depends, then the dependency is optional, which
      --  means that the presence of the depended-upon module is not required,
      --  but that if that module is present, then it must be initialized
      --  before this one.

      Conflicts : String_Lists.List;
      --  The list of modules that cannot be instantiated simultaneously with
      --  this one. Note that if this list has an entry mentioning a virtual
      --  module provided by this module, then there is no conflict unless
      --  another module provides the same virtual module. This allows the
      --  specification of multiple implementations of the same virtual
      --  module that are mutually exclusive.

      Init : Initializer;
      --  The initialization procedure for this module

      Implicit : Boolean;
      --  If this flag is True, then the module is an implicit dependency:
      --  it is added automatically to the dependency list of any module
      --  that is not an implicit dependency itself.

      Shutdown : Finalizer;
      --  The shutdown procedure for this module
   end record;

   procedure Register_Module
     (Info                   : Module_Info);
   --  Register a module described by Info with
   --  the autoconfigurator.

   procedure Initialize_World;
   --  Initialize all modules, respecting the dependencies listed
   --  in each module descriptor.

   procedure Shutdown_World (Wait_For_Completion : Boolean := True);
   --  Shuts down all the modules in reverse initialization order.

   function Is_Initialized return Boolean;
   --  True if, and only if, Initialize_World has been called.

   type Configuration_Hook is access
     function (Section, Key, Default : String)
              return String;

   Get_Conf_Hook : Configuration_Hook := null;
   --  When a configuration subsystem is initialized, it may set this pointer
   --  to a function allowing the logging and initialization subsystems to
   --  retrieve configuration values. This trick is used so PolyORB.Log
   --  and PolyORB.Initialization can be preelaborale.

end PolyORB.Initialization;
