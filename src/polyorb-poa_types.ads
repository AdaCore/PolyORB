------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . P O A _ T Y P E S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If  --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Base types for the Portable Object Adapter.

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;         use PolyORB.Objects;
with PolyORB.Requests;
with PolyORB.Storage_Pools;
with PolyORB.Types; use PolyORB.Types;

with Sequences.Unbounded;

package PolyORB.POA_Types is

   pragma Elaborate_Body;

   Invalid_Object_Id : exception
     renames PolyORB.Obj_Adapters.Invalid_Object_Id;
   Invalid_Method    : exception renames PolyORB.Obj_Adapters.Invalid_Method;

   subtype Time_Stamp is Unsigned_Long;

   --  Base types for the PolyORB POA

   type Obj_Adapter is abstract new PolyORB.Obj_Adapters.Obj_Adapter
      with null record;
   type Obj_Adapter_Access is access all Obj_Adapter'Class;

   type Parameter_Profile_Description is
     access function (Method : PolyORB.Requests.Operation_Id)
     return PolyORB.Any.NVList.Ref;

   type Result_Profile_Description is
     access function (Method : PolyORB.Requests.Operation_Id)
     return PolyORB.Any.Any;

   type Interface_Description is record
      External_Name : Types.String;
      --  External representation of the interface name.

      PP_Desc : Parameter_Profile_Description;
      RP_Desc : Result_Profile_Description;
   end record;

   type Servant is abstract new PolyORB.Objects.Servant with
      record
         If_Desc : Interface_Description;
         --  Description of the most derived interface supported
         --  by this servant. This must be set either by the
         --  personality-specific generated code (for servers
         --  that are statically described in the personality)
         --  or by the user (for dynamic servers).
      end record;
   type Servant_Access is access all Servant'Class;

   package POA_Sequences is new Sequences.Unbounded (Obj_Adapter_Access);
   subtype POAList is POA_Sequences.Sequence;
   type POAList_Access is access all POAList;

   subtype Object_Id is PolyORB.Objects.Object_Id;
   subtype Object_Id_Access is PolyORB.Objects.Object_Id_Access;
   function "=" (X, Y : Object_Id_Access) return Boolean
     renames PolyORB.Objects."=";

   type Unmarshalled_Oid is
     record
         Creator          : Types.String;
         Id               : Types.String;
         System_Generated : Boolean;
         Persistency_Flag : Time_Stamp;
     end record;
   type Unmarshalled_Oid_Access is access Unmarshalled_Oid;
   for Unmarshalled_Oid_Access'Storage_Pool use Storage_Pools.Debug_Pool;

   function "=" (Left, Right : in Servant) return Standard.Boolean
      is abstract;

   function "=" (Left, Right : in Unmarshalled_Oid) return Standard.Boolean;

   function Image
     (Oid : Object_Id) return Types.String;
   --  For debugging purposes.

   function Create_Id
     (Name             : in Types.String;
      System_Generated : in Boolean;
      Persistency_Flag : in Time_Stamp;
      Creator          : in Types.String)
     return Unmarshalled_Oid_Access;
   --  Create an Unmarshalled_Oid

   function Create_Id
     (Name             : in Types.String;
      System_Generated : in Boolean;
      Persistency_Flag : in Time_Stamp;
      Creator          : in Types.String)
     return Object_Id_Access;
   --  Create an Unmarshalled_Oid, and then marshall it into an Object_Id

   function Oid_To_U_Oid
     (Oid : access Object_Id)
     return Unmarshalled_Oid;

--    function Oid_To_U_Oid
--      (Oid : Object_Id)
--      return Unmarshalled_Oid_Access;
--    --  Unmarshall an Object_Id into a Unmarshalled_Oid

   function U_Oid_To_Oid
     (U_Oid : Unmarshalled_Oid)
     return Object_Id_Access;
   --  Marshall an Unmarshalled_Oid into an Object_Id. The caller
   --  is responsible for deallocating the returned Object_Id_Access
   --  after use.

   procedure Free (X : in out PolyORB.POA_Types.Object_Id_Access)
     renames PolyORB.Objects.Free;

   procedure Free is new Ada.Unchecked_Deallocation
     (Unmarshalled_Oid, Unmarshalled_Oid_Access);

end PolyORB.POA_Types;
