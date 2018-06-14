------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . O B J _ A D A P T E R S . S I M P L E           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2017, Free Software Foundation, Inc.          --
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

with Ada.Streams;
with Ada.Unchecked_Conversion;

with PolyORB.Obj_Adapter_QoS;

package body PolyORB.Obj_Adapters.Simple is

   use Ada.Streams;

   use PolyORB.Errors;
   use PolyORB.Tasking.Mutexes;

   use Object_Map_Entry_Arrays;

   subtype Simple_OA_Oid is Stream_Element_Array
     (1 .. Integer'Size / Stream_Element'Size);

   function Index_To_Oid is
      new Ada.Unchecked_Conversion (Integer, Simple_OA_Oid);

   function Oid_To_Index is
      new Ada.Unchecked_Conversion (Simple_OA_Oid, Integer);

   procedure Find_Entry
     (OA    :        Simple_Obj_Adapter;
      Index :        Integer;
      OME   :    out Object_Map_Entry;
      Error : in out PolyORB.Errors.Error_Container);
   --  Check that Index is a valid object Index (associated to a
   --  non-null Servant) for object adapter OA, and return a copy of
   --  the associated entry. If Index is out of range or associated to
   --  a null Servant, Invalid_Object_Id is raised.

   ----------------
   -- Find_Entry --
   ----------------

   procedure Find_Entry
     (OA    :        Simple_Obj_Adapter;
      Index :        Integer;
      OME   :    out Object_Map_Entry;
      Error : in out PolyORB.Errors.Error_Container)
   is
      use type Servants.Servant_Access;

   begin
      Enter (OA.Lock);

      if Index > Last (OA.Object_Map)
        or else OA.Object_Map.Table = null
      then

         --  Going outside limits of the Object Map implies the
         --  Object_Id we are looking for is not valid.

         OME := (Servant => null, If_Desc => (null, null));

      else
         OME := OA.Object_Map.Table (Index);

      end if;

      Leave (OA.Lock);

      if OME.Servant = null then
         Throw (Error,
                Invalid_Object_Id_E,
                Null_Members'(Null_Member));
         OME := (Servant => null, If_Desc => (null, null));
      end if;
   end Find_Entry;

   ------------
   -- Create --
   ------------

   overriding procedure Create (OA : access Simple_Obj_Adapter) is
   begin
      Create (OA.Lock);
      Initialize (OA.Object_Map);
   end Create;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (OA : access Simple_Obj_Adapter) is
   begin
      Destroy (OA.Lock);
      Deallocate (OA.Object_Map);
      Destroy (Obj_Adapter (OA.all)'Access);
   end Destroy;

   ------------
   -- Export --
   ------------

   overriding procedure Export
     (OA    : access Simple_Obj_Adapter;
      Obj   :        Servants.Servant_Access;
      Key   :        Objects.Object_Id_Access;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container)
   is
      use type Servants.Servant_Access;
      use type Objects.Object_Id_Access;

   begin
      if Key /= null
        or else OA.Object_Map.Table = null
      then
         Throw (Error,
                Invalid_Object_Id_E,
                Null_Members'(Null_Member));
         return;
      end if;

      Enter (OA.Lock);
      declare
         New_Id : Integer := Last (OA.Object_Map) + 1;
      begin
         Map :
         for J in First (OA.Object_Map) .. Last (OA.Object_Map) loop
            if OA.Object_Map.Table (J).Servant = null then
               OA.Object_Map.Table (J)
                 := Object_Map_Entry'(Servant => Obj, If_Desc => (null, null));
               New_Id := J;

               exit Map;
            end if;
         end loop Map;

         if New_Id > Last (OA.Object_Map) then
            Increment_Last (OA.Object_Map);
            OA.Object_Map.Table (Last (OA.Object_Map))
              := Object_Map_Entry'(Servant => Obj, If_Desc => (null, null));
         end if;
         Leave (OA.Lock);

         Oid := new Objects.Object_Id'
           (Objects.Object_Id
            (Index_To_Oid (New_Id - First (OA.Object_Map) + 1)));
      end;
   end Export;

   --  XXX There is FAR TOO MUCH code duplication in here!

   --------------
   -- Unexport --
   --------------

   overriding procedure Unexport
     (OA    : access Simple_Obj_Adapter;
      Id    :        Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container)
   is
      use type Servants.Servant_Access;

      Index : constant Integer
        := Oid_To_Index (Simple_OA_Oid (Id.all));

      OME : Object_Map_Entry;

   begin
      --  First, ensure the servant is not null

      Find_Entry (OA.all, Index, OME, Error);

      if Is_Error (Error) then
         return;
      end if;

      pragma Assert (OME.Servant /= null);

      --  then, set to null the entry in object map

      OME := (Servant => null, If_Desc => (null, null));

      Enter (OA.Lock);
      OA.Object_Map.Table (Index) := OME;
      Leave (OA.Lock);
   end Unexport;

   ----------------
   -- Object_Key --
   ----------------

   overriding procedure Object_Key
     (OA      : access Simple_Obj_Adapter;
      Id      :        Objects.Object_Id_Access;
      User_Id :    out Objects.Object_Id_Access;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (OA, Id);

   begin
      Throw (Error,
             Invalid_Object_Id_E,
             Null_Members'(Null_Member));

      --  The Simple Object Adapter does not support
      --  user-defined object identifiers.

      User_Id := null;
   end Object_Key;

   -------------
   -- Get_QoS --
   -------------

   overriding procedure Get_QoS
     (OA    : access Simple_Obj_Adapter;
      Id    :        Objects.Object_Id;
      QoS   :    out PolyORB.QoS.QoS_Parameters;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Id);
      pragma Unreferenced (Error);
   begin
      QoS := PolyORB.Obj_Adapter_QoS.Get_Object_Adapter_QoS (OA);
   end Get_QoS;

   -------------------------------
   -- Set_Interface_Description --
   -------------------------------

   procedure Set_Interface_Description
     (OA      : in out Simple_Obj_Adapter;
      Id      : access Objects.Object_Id;
      If_Desc :        Interface_Description)
   is
      Error : Error_Container;

      Index : constant Integer
        := Oid_To_Index (Simple_OA_Oid (Id.all));

      OME : Object_Map_Entry;

   begin
      Find_Entry (OA, Index, OME, Error);

      if Is_Error (Error) then
         return;
      end if;

      OME.If_Desc := If_Desc;

      Enter (OA.Lock);
      OA.Object_Map.Table (Index) := OME;
      Leave (OA.Lock);
   end Set_Interface_Description;

   ------------------------
   -- Get_Empty_Arg_List --
   ------------------------

   overriding function Get_Empty_Arg_List
     (OA     : access Simple_Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
     return Any.NVList.Ref
   is
      Error : Error_Container;

      Index : constant Integer
        := Oid_To_Index (Simple_OA_Oid (Oid.all));

      OME : Object_Map_Entry;

      Result : Any.NVList.Ref;

   begin
      Find_Entry (OA.all, Index, OME, Error);

      if Is_Error (Error) then
         Catch (Error);
         return Result;
      end if;

      if OME.If_Desc.PP_Desc = null then
         --  No interface information, return empty list

         return Result;
      end if;

      return OME.If_Desc.PP_Desc (Method);
   end Get_Empty_Arg_List;

   ----------------------
   -- Get_Empty_Result --
   ----------------------

   overriding function Get_Empty_Result
     (OA     : access Simple_Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
     return Any.Any
   is
      Error : Error_Container;

      Index : constant Integer
        := Oid_To_Index (Simple_OA_Oid (Oid.all));

      OME : Object_Map_Entry;

      Result : Any.Any;

   begin
      Find_Entry (OA.all, Index, OME, Error);

      if Is_Error (Error) then
         Catch (Error);
         return Result;
      end if;

      if OME.If_Desc.PP_Desc = null then
         --  No interface information, return empty list

         return Result;
      end if;

      return OME.If_Desc.RP_Desc (Method);
   end Get_Empty_Result;

   ------------------
   -- Find_Servant --
   ------------------

   overriding procedure Find_Servant
     (OA      : access Simple_Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant : out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      Index : constant Integer := Oid_To_Index (Simple_OA_Oid (Id.all));
      OME   : Object_Map_Entry;

   begin
      Find_Entry (OA.all, Index, OME, Error);

      if Is_Error (Error) then
         return;
      end if;

      Servant := OME.Servant;
      PolyORB.Servants.Set_Executor (Servant, OA.S_Exec'Access);
   end Find_Servant;

   ---------------------
   -- Release_Servant --
   ---------------------

   overriding procedure Release_Servant
     (OA      : access Simple_Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant : in out Servants.Servant_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Unreferenced (Id);
      pragma Warnings (On);

   begin
      --  SOA: do nothing
      Servant := null;
   end Release_Servant;

end PolyORB.Obj_Adapters.Simple;
