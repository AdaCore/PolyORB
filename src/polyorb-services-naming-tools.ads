------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . S E R V I C E S . N A M I N G . T O O L S         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  with Ada.Finalization;

with PolyORB.References;

with PolyORB.Services.Naming;
with PolyORB.Services.Naming.NamingContext;

package PolyORB.Services.Naming.Tools is

   --  This package allows an object to be chosen either by its IOR or by
   --  its name in the naming service.

   procedure Init (Ref : PolyORB.References.Ref);

   function Locate
     (Name : PolyORB.Services.Naming.Name)
      return PolyORB.References.Ref;

   function Locate
     (Context : PolyORB.Services.Naming.NamingContext.Ref;
      Name    : PolyORB.Services.Naming.Name)
     return PolyORB.References.Ref;
   --  Locate an object given its name, given as an array of name components.

   function Locate
     (IOR_Or_Name : String;
      Sep         : Character := '/')
     return PolyORB.References.Ref;

   function Locate
     (Context     : PolyORB.Services.Naming.NamingContext.Ref;
      IOR_Or_Name : String;
      Sep         : Character                   := '/')
     return PolyORB.References.Ref;
   --  Locate an object by IOR or name. If the string does not start with
   --  "IOR:", the name will be parsed before it is looked up, using
   --  Parse_Name below.

   procedure Register
     (Name   : in String;
      Ref    : in PolyORB.References.Ref;
      Rebind : in Boolean := False;
      Sep    : in Character := '/');
   --  Register an object by its name by binding or rebinding.
   --  The name will be parsed by Parse_Name below; any necessary contexts
   --  will be created on the name server.
   --  If Rebind is True, then a rebind will be performed if the name
   --  is already bound.

   procedure Unregister (Name : in String);
   --  Unregister an object by its name by unbinding it.

   function Parse_Name
     (Name : String;
      Sep : Character := '/')
     return PolyORB.Services.Naming.Name;
   --  Split a sequence of name component specifications separated
   --  with Sep characters into a name component array. Any leading
   --  Sep is ignored.

end PolyORB.Services.Naming.Tools;
