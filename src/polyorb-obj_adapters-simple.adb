------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . O B J _ A D A P T E R S . S I M P L E           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Object adapters: entities that manage the association
--  of references with servants.

--  $Id$

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Conversion;

package body PolyORB.Obj_Adapters.Simple is

   use PolyORB.Soft_Links;
   use Object_Map_Entry_Seqs;

   subtype Simple_OA_Oid is Stream_Element_Array
     (1 .. Integer'Size / Stream_Element'Size);

   function Index_To_Oid is
      new Ada.Unchecked_Conversion (Integer, Simple_OA_Oid);
   function Oid_To_Index is
      new Ada.Unchecked_Conversion (Simple_OA_Oid, Integer);

   function Find_Entry
     (OA    : Simple_Obj_Adapter;
      Index : Integer)
     return Object_Map_Entry;
   --  Check that Index is a valid object Index (associated
   --  to a non-null Servant) for object adapter OA, and
   --  return the associated entry. If Index is out of range
   --  or associated to a null Servant, Invalid_Object_Id is raised.

   function Find_Entry
     (OA    : Simple_Obj_Adapter;
      Index : Integer)
   return Object_Map_Entry is
   begin
      declare
         OME : constant Object_Map_Entry
           := Element_Of (OA.Object_Map, Index);
      begin
         if OME.Servant = null then
            raise Invalid_Object_Id;
         end if;

         return OME;
      end;
   exception
      when Sequences.Index_Error =>
         raise Invalid_Object_Id;
      when others =>
         raise;
   end Find_Entry;

   --  XXX Replace OA.Lock with a r/w lock???

   procedure Create (OA : access Simple_Obj_Adapter) is
   begin
      Create (OA.Lock);
   end Create;

   procedure Destroy (OA : access Simple_Obj_Adapter) is
   begin
      Destroy (OA.Lock);
   end Destroy;

   function Export
     (OA  : access Simple_Obj_Adapter;
      Obj : Objects.Servant_Access)
     return Object_Id is
   begin
      Enter (OA.Lock);
      declare
         M : constant Element_Array := To_Element_Array (OA.Object_Map);
         New_Id : Integer := M'Last + 1;
      begin
         Map :
         for I in M'Range loop
            if M (I).Servant = null then
               Replace_Element
                 (OA.Object_Map,
                  1 + I - M'First,
                  Object_Map_Entry'
                  (Servant => Obj,
                   If_Desc => (null, null)));
               New_Id := I;
               exit Map;
            end if;
         end loop Map;

         if New_Id > M'Last then
            Append (OA.Object_Map, Object_Map_Entry'
                    (Servant => Obj, If_Desc => (null, null)));
         end if;
         Leave (OA.Lock);

         return Object_Id (Index_To_Oid (New_Id - M'First + 1));
      end;
   end Export;

   --  XXX There is FAR TOO MUCH code duplication in here!

   procedure Unexport
     (OA : access Simple_Obj_Adapter;
      Id : Object_Id)
   is
      Index : constant Integer := Oid_To_Index (Simple_OA_Oid (Id));
   begin
      Enter (OA.Lock);

      begin
         declare
            OME : Object_Map_Entry
              := Find_Entry (OA.all, Index);
         begin
            pragma Assert (OME.Servant /= null);
            OME := (Servant => null, If_Desc => (null, null));
            Replace_Element (OA.Object_Map, Index, OME);
         end;
      exception
         when others =>
            Leave (OA.Lock);
            raise;
      end;

      Leave (OA.Lock);
   end Unexport;

   procedure Set_Interface_Description
     (OA      : in out Simple_Obj_Adapter;
      Id      : Object_Id;
      If_Desc : Interface_Description)
   is
      Index : constant Integer := Oid_To_Index (Simple_OA_Oid (Id));
   begin
      Enter (OA.Lock);

      begin
         declare
            OME : Object_Map_Entry
              := Find_Entry (OA, Index);
         begin
            pragma Assert (OME.Servant /= null);
            OME.If_Desc := If_Desc;
            Replace_Element (OA.Object_Map, Index, OME);
         end;
      exception
         when others =>
            Leave (OA.Lock);
            raise;
      end;

      Leave (OA.Lock);
   end Set_Interface_Description;

   function Get_Empty_Arg_List
     (OA     : access Simple_Obj_Adapter;
      Oid    : Object_Id;
      Method : Requests.Operation_Id)
     return Any.NVList.Ref
   is
      Index : constant Integer := Oid_To_Index (Simple_OA_Oid (Oid));
      Result : Any.NVList.Ref;
   begin
      Enter (OA.Lock);

      begin
         declare
            OME : Object_Map_Entry
              := Find_Entry (OA.all, Index);
         begin
            if OME.If_Desc.PP_Desc = null then
               raise Invalid_Method;
            end if;
            Result := OME.If_Desc.PP_Desc (Method);
         end;
      exception
         when others =>
            Leave (OA.Lock);
            raise;
      end;

      Leave (OA.Lock);
      return Result;
   end Get_Empty_Arg_List;

   function Get_Empty_Result
     (OA     : access Simple_Obj_Adapter;
      Oid    : Object_Id;
      Method : Requests.Operation_Id)
     return Any.Any
   is
      Index : constant Integer := Oid_To_Index (Simple_OA_Oid (Oid));
      Result : Any.Any;
   begin
      Enter (OA.Lock);

      begin
         declare
            OME : Object_Map_Entry
              := Find_Entry (OA.all, Index);
         begin
            if OME.If_Desc.PP_Desc = null then
               raise Invalid_Method;
            end if;

            Result := OME.If_Desc.RP_Desc (Method);
         end;
      exception
         when others =>
            Leave (OA.Lock);
            raise;
      end;

      Leave (OA.Lock);
      return Result;
   end Get_Empty_Result;

   function Find_Servant
     (OA : access Simple_Obj_Adapter;
      Id : Object_Id)
     return Servant_Access
   is
      Result : Servant_Access;
   begin
      Enter (OA.Lock);
      Result := Element_Of (OA.Object_Map, Oid_To_Index
                            (Simple_OA_Oid (Id))).Servant;
      Leave (OA.Lock);
      return Result;
   end Find_Servant;

   procedure Release_Servant
     (OA : access Simple_Obj_Adapter;
      Id : Object_Id;
      Servant : in out Servant_Access) is
   begin
      --  SOA: do nothing.
      Servant := null;
   end Release_Servant;

end PolyORB.Obj_Adapters.Simple;
