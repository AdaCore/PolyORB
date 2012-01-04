------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     D E L E G A T E D _ S E R V E R                      --
--                                                                          --
--                                 S p e c                                  --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Object;
with Echo.Delegate;

package Delegated_Server is

   function Echo_With_Delegation
     (Self : access Integer;
      Mesg : CORBA.String)
     return CORBA.String;
   --  Function that will be called when a delegation is used. Note that
   --  the Self integer has no meaning here, but a Self is mandatory.

   package Delegated is new Echo.Delegate
     (Wrapped    => Integer,
      echoString => Echo_With_Delegation);
   --  The package Echo.Delegate is instantiated, with the dummy Integer
   --  type as the "real" type.

   Dummy : aliased Integer;
   --  A dummy "object"

end Delegated_Server;
