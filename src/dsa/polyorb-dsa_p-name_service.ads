------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . D S A _ P . N A M E _ S E R V I C E            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2010-2013, Free Software Foundation, Inc.          --
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

--  This package implements the abstract name context primitives, that
--  permit the dispatching between different concrete contexts.

with PolyORB.References;

package PolyORB.DSA_P.Name_Service is

   type Name_Server is abstract tagged record
      Base_Ref : PolyORB.References.Ref;
   end record;
   --  The abstract type used to disptach Nameserver_Lookup/
   --  Nameserver_Register. The Stringified_Ref is used only on client's side,
   --  it is assigned by Initialize_MDNS_Context and is used in
   --  Nameserver_Lookup in order to initialize the remote Base_Ref field.

   type Name_Server_Access is access all Name_Server'Class;
   Name_Ctx : PolyORB.DSA_P.Name_Service.Name_Server_Access;

   procedure Initialize
     (Name_Ctx : access Name_Server;
      Location : String) is abstract;
   --  Initialize naming context from given location information

   function Nameserver_Lookup
     (Name_Ctx : access Name_Server;
      Name     : String;
      Kind     : String;
      Initial  : Boolean := True) return PolyORB.References.Ref is abstract;
   --  Look up Name. Initial is set True for an initial lookup, False for a
   --  subsequent lookup to refresh a reference that has become invalid.
   --  In the first case lookup is retried until a valid reference is obtained,
   --  and raise an exception if maximum retry count is reached, else just
   --  return an empty ref if name server retruns an empty or invalid result.

   procedure Nameserver_Register
     (Name_Ctx : access Name_Server;
      Name     : String;
      Kind     : String;
      Obj      : PolyORB.References.Ref) is abstract;
   --  Register Obj as associated with the given name and kind

   procedure Initialize_Name_Server;
   --  Called by the System.Partition_Interface.Initialize procedure, during
   --  partition's elaboration. Depending on the current configuration,
   --  sets the Name Context to mDNS or COS_Naming and the Base_Ref to
   --  the corresponding remote reference.

   function Get_Name_Server return Name_Server_Access;
   --  Retrieves the name context, used by System.Partition_Interface

   --------------------------------------------
   -- RCI lookup and reconnection management --
   --------------------------------------------

   Time_Between_Requests : Duration := 1.0;
   Max_Requests          : Natural := 10;
   --  These are the initial and default values

   type Reconnection_Policy_Type is
     (Fail_Until_Restart, Block_Until_Restart, Reject_On_Restart);
   Default_Reconnection_Policy : constant Reconnection_Policy_Type :=
                                   Fail_Until_Restart;

   function Get_Reconnection_Policy
     (Name : String) return Reconnection_Policy_Type;
   --  Retrieve reconnection policy for this RCI from runtime parameters
   --  set by gnatdist.

   type RCI_Attribute is (Local, Reconnection);

   function RCI_Attr (Name : String; Attr : RCI_Attribute) return String;

   function Is_Reference_Valid (R : PolyORB.References.Ref) return Boolean;
   --  Binds a reference to determine whether it is valid

end PolyORB.DSA_P.Name_Service;
