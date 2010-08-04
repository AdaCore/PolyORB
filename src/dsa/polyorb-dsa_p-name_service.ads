------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . D S A _ P . N A M E _ S E R V I C E            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2010, Free Software Foundation, Inc.             --
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

with PolyORB.References;
with PolyORB.Types;

package PolyORB.DSA_P.Name_Service is

   type Name_Context is abstract tagged record
      Base_Ref : PolyORB.References.Ref;
      Stringified_Ref : PolyORB.Types.String;
   end record;
   --  The abstract type used to disptach Nameserver_Lookup/
   --  Nameserver_Register. The Stringified_Ref is used only on client's side
   --  , it is assigned by Initialize_MDNS_Context and is used in
   --  Nameserver_Lookup in order to initialize the remote Base_Ref field.

   type Name_Context_Access is access all Name_Context'Class;

   function Nameserver_Lookup
     (Name_Ctx : access Name_Context;
      Name     : String;
      Kind     : String;
      Initial  : Boolean := True) return PolyORB.References.Ref is abstract;
   --  abstract declaration of Nameserver_Lookup

   procedure Nameserver_Register
     (Name_Ctx : access Name_Context;
      Name : String;
      Kind : String;
      Obj  : PolyORB.References.Ref) is abstract;
   --  abstract declaration of Nameserver_Register

   procedure Initialize_Name_Context (Name_Ctx : in out Name_Context_Access);
   --  Called by the System.Partition_Interface.Initialize procedure, during
   --  partition's elaboration. Depending on the current configuration,
   --  sets the Name Context to mDNS or COS_Naming and the Base_Ref to
   --  the corresponding remote reference.

end PolyORB.DSA_P.Name_Service;
