------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . P O A _ T Y P E S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

--  Base types for the Portable Object Adapter.

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.Types;
with PolyORB.Utils.HFunctions.Mul;
with PolyORB.Utils.HTables.Perfect;

with PolyORB.Sequences.Unbounded;

package PolyORB.POA_Types is

   pragma Elaborate_Body;

   use PolyORB.Objects;
   use PolyORB.Types;

   Invalid_Object_Id : exception
     renames PolyORB.Obj_Adapters.Invalid_Object_Id;
   Invalid_Method    : exception renames PolyORB.Obj_Adapters.Invalid_Method;

   ----------------
   -- Time_Stamp --
   ----------------

   subtype Time_Stamp is Unsigned_Long;

   Null_Time_Stamp : constant Time_Stamp;
   --  A time marker.

   subtype Lifespan_Cookie is Unsigned_Long;
   --  A piece of information embedded in an object id by the lifespan
   --  policy for control of reference validity across ORB executions.

   -----------------
   -- Obj_Adapter --
   -----------------

   type Obj_Adapter is abstract new PolyORB.Obj_Adapters.Obj_Adapter
      with null record;
   type Obj_Adapter_Access is access all Obj_Adapter'Class;

   ----------------------------------
   -- Object Interface description --
   ----------------------------------

   type Parameter_Profile_Description is
     access function (Method : String)
     return PolyORB.Any.NVList.Ref;

   type Result_Profile_Description is
     access function (Method : String)
     return PolyORB.Any.Any;

   type Interface_Description is record
      External_Name : Types.String;
      --  External representation of the interface name.

      PP_Desc : Parameter_Profile_Description;
      RP_Desc : Result_Profile_Description;
   end record;

   -------------
   -- POAList --
   -------------

   package POA_Sequences is new PolyORB.Sequences.Unbounded
     (Obj_Adapter_Access);
   subtype POAList is POA_Sequences.Sequence;
   type POAList_Access is access all POAList;

   --------------
   -- POATable --
   --------------

   package POA_HTables is new PolyORB.Utils.HTables.Perfect
     (Obj_Adapter_Access,
      PolyORB.Utils.HFunctions.Mul.Hash_Mul_Parameters,
      PolyORB.Utils.HFunctions.Mul.Default_Hash_Parameters,
      PolyORB.Utils.HFunctions.Mul.Hash,
      PolyORB.Utils.HFunctions.Mul.Next_Hash_Parameters);
   subtype POATable is POA_HTables.Table_Instance;
   type POATable_Access is access all POATable;

   procedure Free is new Ada.Unchecked_Deallocation
     (POATable, POATable_Access);

   ----------------
   -- Object Ids --
   ----------------

   subtype Object_Id is PolyORB.Objects.Object_Id;
   subtype Object_Id_Access is PolyORB.Objects.Object_Id_Access;
   function "=" (X, Y : Object_Id_Access) return Boolean
     renames PolyORB.Objects."=";

   type Unmarshalled_Oid is record
      Id               : Types.String;     --  Id
      Creator          : Types.String;     --  Creator 'name'
      System_Generated : Boolean;          --  System or User managed ?
      Persistency_Flag : Lifespan_Cookie;  --  Object's Lifespan

      --  NOTE:
      --   * the 'Creator' is typically the POA to which the object is
      --     attached,

   end record;
   type Unmarshalled_Oid_Access is access Unmarshalled_Oid;

   procedure Free is new Ada.Unchecked_Deallocation
     (Unmarshalled_Oid, Unmarshalled_Oid_Access);

   function "=" (Left, Right : in Unmarshalled_Oid)
                return Standard.Boolean;

   function Image
     (Oid : Object_Id) return Types.String;
   --  For debugging purposes.

   function Create_Id
     (Name             : in Types.String;
      System_Generated : in Boolean;
      Persistency_Flag : in Time_Stamp;
      Creator          : in Types.String)
     return Unmarshalled_Oid_Access;
   pragma Inline (Create_Id);
   --  Create an Unmarshalled_Oid_Access.

   function Create_Id
     (Name             : in Types.String;
      System_Generated : in Boolean;
      Persistency_Flag : in Time_Stamp;
      Creator          : in Types.String)
     return Unmarshalled_Oid;
   pragma Inline (Create_Id);
   --  Create an Unmarshalled_Oid.

   function Create_Id
     (Name             : in Types.String;
      System_Generated : in Boolean;
      Persistency_Flag : in Time_Stamp;
      Creator          : in Types.String)
     return Object_Id_Access;
   pragma Inline (Create_Id);
   --  Create an Unmarshalled_Oid, and then marshall it into an Object_Id

   function Get_Name (Oid : Object_Id) return Types.String;
   --  Return 'Name' component marshalled in Oid.

   function Oid_To_U_Oid
     (Oid : access Object_Id)
     return Unmarshalled_Oid;
   --  Unmarshall an Object_Id into a Unmarshalled_Oid

   function Oid_To_U_Oid
     (Oid : Object_Id)
     return Unmarshalled_Oid;
   --  Unmarshall an Object_Id into a Unmarshalled_Oid

   function U_Oid_To_Oid
     (U_Oid : Unmarshalled_Oid)
     return Object_Id_Access;
   --  Marshall an Unmarshalled_Oid into an Object_Id. The caller
   --  is responsible for deallocating the returned Object_Id_Access
   --  after use.

   function U_Oid_To_Oid
     (U_Oid : Unmarshalled_Oid)
     return Object_Id;
   --  Marshall an Unmarshalled_Oid into an Object_Id.

   procedure Free (X : in out PolyORB.POA_Types.Object_Id_Access)
     renames PolyORB.Objects.Free;

private

   Null_Time_Stamp : constant Time_Stamp := 0;

end PolyORB.POA_Types;
