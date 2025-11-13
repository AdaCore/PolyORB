------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . P O A _ T Y P E S                     --
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

pragma Ada_2012;

--  Base types for the Portable Object Adapter.

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Errors;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.Servants;
with PolyORB.Smart_Pointers;
with PolyORB.Types;
with PolyORB.Utils.HFunctions.Hyper;
with PolyORB.Utils.HTables.Perfect;
with PolyORB.Utils.Chained_Lists;

package PolyORB.POA_Types is

   pragma Elaborate_Body;

   use PolyORB.Objects;
   use PolyORB.Types;

   ----------------
   -- Time_Stamp --
   ----------------

   subtype Time_Stamp is Duration;

   Null_Time_Stamp : constant Time_Stamp;
   --  A time marker.

   subtype Lifespan_Cookie is Time_Stamp;
   --  A piece of information embedded in an object id by the lifespan policy
   --  for control of reference validity across ORB executions.

   -----------------
   -- Obj_Adapter --
   -----------------

   type Obj_Adapter is abstract new PolyORB.Obj_Adapters.Obj_Adapter
      with null record;
   type Obj_Adapter_Access is access all Obj_Adapter'Class;

   type Obj_Adapter_Ref is new PolyORB.Smart_Pointers.Ref with null record;

   Null_POA_Ref : Obj_Adapter_Ref;

   ----------------------------------
   -- Object Interface description --
   ----------------------------------

   type Parameter_Profile_Description is
     access function (Method : String) return PolyORB.Any.NVList.Ref;

   type Result_Profile_Description is
     access function (Method : String) return PolyORB.Any.Any;

   type Interface_Description is record
      PP_Desc : Parameter_Profile_Description;
      RP_Desc : Result_Profile_Description;
   end record;

   --------------
   -- POATable --
   --------------

   package POA_HTables is new PolyORB.Utils.HTables.Perfect
     (Obj_Adapter_Ref,
      PolyORB.Utils.HFunctions.Hyper.Hash_Hyper_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Default_Hash_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Hash,
      PolyORB.Utils.HFunctions.Hyper.Next_Hash_Parameters);

   subtype POATable is POA_HTables.Table_Instance;
   type POATable_Access is access all POATable;

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => POATable, Name => POATable_Access);

   -------------
   -- POAList --
   -------------

   package POA_Lists is
      new PolyORB.Utils.Chained_Lists (Obj_Adapter_Ref, "=", True);
   subtype POAList is POA_Lists.List;

   ----------------
   -- Object Ids --
   ----------------

   POA_Path_Separator : constant Character := '/';

   subtype Object_Id is PolyORB.Objects.Object_Id;
   subtype Object_Id_Access is PolyORB.Objects.Object_Id_Access;

   function "=" (X, Y : Object_Id_Access) return Boolean
     renames PolyORB.Objects."=";

   type Unmarshalled_Oid is record
      Id               : Types.String;
      --  Object id within POA

      Creator          : Types.String;
      --  Creator (POA path delimited with POA_Path_Separator)

      System_Generated : Boolean;
      --  System or User managed ?

      Persistency_Flag : Lifespan_Cookie;
      --  Object's Lifespan
   end record;

   overriding function "="
     (Left, Right : Unmarshalled_Oid)
     return Standard.Boolean;

   type Unmarshalled_Oid_Access is access Unmarshalled_Oid;

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Unmarshalled_Oid, Name => Unmarshalled_Oid_Access);

   function Create_Id
     (Name             : Standard.String;
      System_Generated : Boolean;
      Persistency_Flag : Time_Stamp;
      Creator          : Standard.String) return Unmarshalled_Oid_Access;
   pragma Inline (Create_Id);
   --  Create an Unmarshalled_Oid_Access.

   function Create_Id
     (Name             : Standard.String;
      System_Generated : Boolean;
      Persistency_Flag : Time_Stamp;
      Creator          : Standard.String) return Unmarshalled_Oid;
   pragma Inline (Create_Id);
   --  Create an Unmarshalled_Oid.

   function Create_Id
     (Name             : Standard.String;
      System_Generated : Boolean;
      Persistency_Flag : Time_Stamp;
      Creator          : Standard.String) return Object_Id_Access;
   pragma Inline (Create_Id);
   --  Create an Unmarshalled_Oid, and then marshall it into an Object_Id

   procedure Oid_To_U_Oid
     (Oid   :        Object_Id;
      U_Oid :    out Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container);
   --  Unmarshall an Object_Id into a Unmarshalled_Oid

   function Get_Creator (Oid : Object_Id) return String;
   --  Return Creator name coded in Oid

   function U_Oid_To_Oid (U_Oid : Unmarshalled_Oid) return Object_Id_Access;
   --  Marshall an Unmarshalled_Oid into an Object_Id. The caller
   --  is responsible for deallocating the returned Object_Id_Access
   --  after use.

   function U_Oid_To_Oid (U_Oid : Unmarshalled_Oid) return Object_Id;
   --  Marshall an Unmarshalled_Oid into an Object_Id.

   procedure Free (X : in out PolyORB.POA_Types.Object_Id_Access)
     renames PolyORB.Objects.Free;

   --------------------------
   -- POA Callback objects --
   --------------------------

   --  AdapterActivator

   type AdapterActivator is abstract new Smart_Pointers.Ref with null record;

   type AdapterActivator_Access is access all AdapterActivator'Class;

   procedure Unknown_Adapter
     (Self   : access AdapterActivator;
      Parent : access Obj_Adapter'Class;
      Name   : String;
      Result :    out Boolean;
      Error  : in out PolyORB.Errors.Error_Container) is abstract;

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => AdapterActivator'Class, Name => AdapterActivator_Access);

   --  Servant Manager

   type ServantManager is abstract new Smart_Pointers.Ref with null record;

   type ServantManager_Access is access all ServantManager'Class;

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => ServantManager'Class, Name => ServantManager_Access);

   --  Servant Activator

   type ServantActivator is abstract new ServantManager with null record;

   type ServantActivator_Access is access all ServantActivator'Class;

   procedure Incarnate
     (Self    : access ServantActivator;
      Oid     : Object_Id;
      Adapter : access Obj_Adapter'Class;
      Returns :    out PolyORB.Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  The Error argument used only for processing location forwarding, thus
   --  the only valid Error.Kind is ForwardRequest_E.

   procedure Etherealize
     (Self                  : access ServantActivator;
      Oid                   : Object_Id;
      Adapter               : access Obj_Adapter'Class;
      Serv                  : PolyORB.Servants.Servant_Access;
      Cleanup_In_Progress   : Boolean;
      Remaining_Activations : Boolean) is abstract;

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => ServantActivator'Class, Name => ServantActivator_Access);

   --  Servant Locator

   type ServantLocator is abstract new ServantManager with null record;

   type ServantLocator_Access is access all ServantLocator'Class;

   type Cookie_Base is tagged null record;
   --  User defined cookie type

   type Cookie is access all Cookie_Base'Class;

   procedure Preinvoke
     (Self       : access ServantLocator;
      Oid        : Object_Id;
      Adapter    : access Obj_Adapter'Class;
      Operation  : PolyORB.Types.Identifier;
      The_Cookie :    out Cookie;
      Returns    :    out PolyORB.Servants.Servant_Access;
      Error      : in out PolyORB.Errors.Error_Container) is abstract;
   --  The Error argument used only for processing location forwarding, thus
   --  the only valid Error.Kind is ForwardRequest_E.

   procedure Postinvoke
     (Self        : access ServantLocator;
      Oid         : Object_Id;
      Adapter     : access Obj_Adapter'Class;
      Operation   : PolyORB.Types.Identifier;
      The_Cookie  : Cookie;
      The_Servant : PolyORB.Servants.Servant_Access) is abstract;

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => ServantLocator'Class, Name => ServantLocator_Access);

private

   Null_Time_Stamp : constant Time_Stamp := Time_Stamp'First;

end PolyORB.POA_Types;
