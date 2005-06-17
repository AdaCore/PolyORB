------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ B A C K                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1995-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--                   GLADE  is maintained by AdaCore                        --
--                      (email: sales@adacore.com)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains all the routines to generate stubs (object
--  files), skeleton files (object files), PCS units (source and
--  object files) and eventually executable files.

package XE_Back is

   type Backend is abstract tagged limited private;
   type Backend_Access is access all Backend'Class;

   procedure Set_PCS_Dist_Flags (Self : access Backend) is abstract;
   --  Set PCS-specific default command line flags

   procedure Initialize (Self : access Backend) is abstract;
   --  Initialize the backend

   procedure Run_Backend (Self : access Backend) is abstract;
   --  Generate stubs, skels, PCS units and executables.

   function Find_Backend (PCS_Name : String) return Backend_Access;
   --  Return an instance of the backend appropriate for the specified PCS

private

   type Backend is abstract tagged limited null record;

   procedure Register_Backend
     (PCS_Name : String; The_Backend : Backend_Access);

end XE_Back;
