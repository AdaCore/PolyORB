------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . C O R B A _ P . N A M I N G _ T O O L S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Wrappers for the COS Naming service to facilitate retrievel of object
--  references by IOR or by name.

with Ada.Finalization;

with CORBA.Object;
with CosNaming.NamingContext;

package PolyORB.CORBA_P.Naming_Tools is

   function Locate (Name : CosNaming.Name) return CORBA.Object.Ref;

   function Locate
     (Context : CosNaming.NamingContext.Ref;
      Name    : CosNaming.Name) return CORBA.Object.Ref;
   --  Locate an object given its name, given as an array of name components.

   function Locate
     (IOR_Or_Name : String; Sep : Character := '/') return CORBA.Object.Ref;

   function Locate
     (Context     : CosNaming.NamingContext.Ref;
      IOR_Or_Name : String;
      Sep         : Character := '/') return CORBA.Object.Ref;
   --  Locate an object by IOR or name. If the string does not start with
   --  "IOR:", the name will be parsed before it is looked up, using
   --  Parse_Name below.

   procedure Register
     (Name   : String;
      Ref    : CORBA.Object.Ref;
      Rebind : Boolean := False;
      Sep    : Character := '/');
   --  Register an object by its name by binding or rebinding.
   --  The name will be parsed by Parse_Name below; any necessary contexts
   --  will be created on the name server.
   --  If Rebind is True, then a rebind will be performed if the name
   --  is already bound.

   procedure Unregister (Name : String);
   --  Unregister an object by its name by unbinding it

   type Server_Guard is limited private;
   procedure Register
     (Guard  : in out Server_Guard;
      Name   : String;
      Ref    : CORBA.Object.Ref;
      Rebind : Boolean := False;
      Sep    : Character := '/');
   --  A Server_Guard object is an object which is able to register a server
   --  reference in a naming service (see Register above), and destroy this
   --  name using Unregister when the object disappears (the program terminates
   --  or the Server_Guard object lifetime has expired).

   function Parse_Name
     (Name : String;
      Sep : Character := '/') return CosNaming.Name;
   --  Split a sequence of name component specifications separated with Sep
   --  characters into a name component array. Any leading Sep is ignored.

private

   type Server_Guard is new Ada.Finalization.Limited_Controlled with record
      Name : CORBA.String := CORBA.To_CORBA_String ("");
   end record;

   procedure Finalize (Guard : in out Server_Guard);

end PolyORB.CORBA_P.Naming_Tools;
