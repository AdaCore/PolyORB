------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      M O M A . R E F E R E N C E S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with MOMA.Types;

package MOMA.References is

   procedure Initialize_Naming_Service (Naming_Ref : Standard.String);
   --  Initialize Naming Service using Naming_Ref stringified reference

   procedure Register_Name
     (Name   : String;
      Ref    : MOMA.Types.Ref;
      Rebind : Boolean := False;
      Sep    : Character := '/');
   --  Register an object by its name by binding or rebinding.
   --  If Rebind is True, then a rebind will be performed if the name
   --  is already bound.

   function Locate
     (IOR_Or_Name : String;
      Sep         : Character := '/')
     return MOMA.Types.Ref;
   --  Locate an object by IOR or name. If the string does not start with
   --  "IOR:", the name will be parsed before it is looked up.

   function Reference_To_IOR_String
     (Ref : MOMA.Types.Ref)
     return Standard.String;

   procedure String_To_Reference
     (S   : Standard.String;
      Ref : out MOMA.Types.Ref);

end MOMA.References;
