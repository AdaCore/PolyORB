------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . G A R L I C . D E B U G                    --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GARLIC is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

package System.Garlic.Debug is

   --  This package is to be used for debugging purpose. All the subprograms
   --  in it are tagged as Inline, and these calls expand to nothing if
   --  assertions are turned off (no -gnata).
   --
   --  This package should be used as follow:
   --
   --    Private_Debug_Key : constant Debug_Key :=
   --      Debug_Initialize ("GARLIC", "(s-garlic): ");
   --    procedure D
   --      (Level   : in Debug_Levels;
   --       Message : in String;
   --       Key     : in Debug_Key := Private_Debug_Key)
   --      renames Print_Debug_Info;
   --
   --  Then, later in the code, you can do:
   --    D (D_Elaborate, "Elaboration terminated");

   pragma Elaborate_Body;
   --  Since this package may be used during elaboration, the body must
   --  be elaborated as soon as possible.

   type Debug_Levels is
      (D_Communication,         --  Communication stuff,
       D_Debug,                 --  Misc
       D_Dump,                  --  Dump packets
       D_Elaborate,             --  Elaboration
       D_Exception,             --  Exceptions
       D_Garlic,                --  GARLIC internal
       D_Interface,             --  Interface with C
       D_Lookup,                --  Unit lookup
       D_Low,                   --  Low level subprograms
       D_Module,                --  Protocol module
       D_Protocol,              --  Queries/Answer
       D_Register,              --  Unit registration
       D_Request,               --  RPC request
       D_RNS,                   --  RNS internal
       D_Server,                --  Server
       D_Table,                 --  Tables
       D_Terminate,             --  Termination
       D_Warning,               --  Warning
       No_Debug);               --  Internal -- do NOT use
   --  Debug levels.

   Debug_Letters : constant array (Debug_Levels) of Character :=
     (D_Communication => 'C',
      D_Debug         => 'D',
      D_Dump          => 'X',
      D_Elaborate     => 'E',
      D_Exception     => 'F',
      D_Garlic        => 'G',
      D_Interface     => 'I',
      D_Lookup        => 'K',
      D_Low           => 'L',
      D_Module        => 'M',
      D_Protocol      => 'P',
      D_Register      => 'R',
      D_Request       => 'Q',
      D_RNS           => 'Y',
      D_Server        => 'S',
      D_Table         => 'T',
      D_Terminate     => 'Z',
      D_Warning       => 'W',
      No_Debug        => ' ');
   --  Letters used to turn debugging on for a given topic.

   type Debug_Key is private;
   --  The key used for further references to the variable.

   function Debug_Initialize
     (Variable : String;
      Banner   : String)
      return Debug_Key;
   pragma Inline (Debug_Initialize);
   --  Debug_Initialize is called with two strings. The first one is the name
   --  of the environment variable to look for, the second one is the
   --  banner to use when printing debugging information.

   procedure Print_Debug_Info
     (Level   : in Debug_Levels;
      Message : in String;
      Key     : in Debug_Key);
   pragma Inline (Print_Debug_Info);
   --  This procedure prints debugging information if the given flag was
   --  set in the right environment variable.

   function Debug_Mode
     (Level : Debug_Levels;
      Key   : Debug_Key)
      return Boolean;
   pragma Inline (Debug_Mode);
   --  Return true if this level is active.

private

   type Debug_Key is new Natural;

end System.Garlic.Debug;
