------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . O B J _ A D A P T E R S . S I M P L E           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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

with Ada.Streams;
with Ada.Unchecked_Conversion;

package body PolyORB.Obj_Adapters.Simple is

   use Ada.Streams;

   use PolyORB.Exceptions;
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
      Error : in out PolyORB.Exceptions.Error_Container);
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
      Error : in out PolyORB.Exceptions.Error_Container)
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

   ------------------------------
   -- Handle_Request_Execution --
   ------------------------------

   function Handle_Request_Execution
     (Self      : access Simple_Executor;
      Msg       : PolyORB.Components.Message'Class;
      Requestor : PolyORB.Components.Component_Access)
     return PolyORB.Components.Message'Class
   is
      use PolyORB.Servants;

      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      --  At this stage, PolyORB.ORB.Run has already affected a thread
      --  to handle the request execution, in which this current call
      --  is executed. Thus we just need to call the Execute_Servant
      --  procedure to go on with the request execution.

      return Execute_Servant (Servant_Access (Requestor), Msg);
   end Handle_Request_Execution;

   ------------
   -- Create --
   ------------

   procedure Create (OA : access Simple_Obj_Adapter) is
   begin
      Create (OA.Lock);
      Initialize (OA.Object_Map);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (OA : access Simple_Obj_Adapter) is
   begin
      Destroy (OA.Lock);
      Deallocate (OA.Object_Map);
      Destroy (Obj_Adapter (OA.all)'Access);
   end Destroy;

   ------------
   -- Export --
   ------------

   procedure Export
     (OA    : access Simple_Obj_Adapter;
      Obj   :        Servants.Servant_Access;
      Key   :        Objects.Object_Id_Access;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Exceptions.Error_Container)
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

   procedure Unexport
     (OA    : access Simple_Obj_Adapter;
      Id    :        Objects.Object_Id_Access;
      Error : in out PolyORB.Exceptions.Error_Container)
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

   procedure Object_Key
     (OA      : access Simple_Obj_Adapter;
      Id      :        Objects.Object_Id_Access;
      User_Id :    out Objects.Object_Id_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (OA, Id);
      pragma Warnings (On); --  WAG:3.15

      use PolyORB.Exceptions;

   begin
      Throw (Error,
             Invalid_Object_Id_E,
             Null_Members'(Null_Member));

      --  The Simple Object Adapter does not support
      --  user-defined object identifiers.

      User_Id := null;
   end Object_Key;

   -------------------------------
   -- Set_Interface_Description --
   -------------------------------

   procedure Set_Interface_Description
     (OA      : in out Simple_Obj_Adapter;
      Id      : access Objects.Object_Id;
      If_Desc :        Interface_Description)
   is
      use type Servants.Servant_Access;

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

   function Get_Empty_Arg_List
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

   function Get_Empty_Result
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

   procedure Find_Servant
     (OA      : access Simple_Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
      Index : constant Integer
        := Oid_To_Index (Simple_OA_Oid (Id.all));

      OME : Object_Map_Entry;

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

   procedure Release_Servant
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
