--  Object adapters: entities that manage the association
--  of references with servants.

--  $Id$

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Conversion;

package body Droopi.Obj_Adapters.Simple is

   use Droopi.Soft_Links;
   use Object_Map_Entry_Seqs;

   subtype Simple_OA_Oid is Stream_Element_Array
     (1 .. Integer'Size / Stream_Element'Size);

   function Index_To_Oid is
      new Ada.Unchecked_Conversion (Integer, Simple_OA_Oid);
   function Oid_To_Index is
      new Ada.Unchecked_Conversion (Simple_OA_Oid, Integer);


   --  XXX Replace OA.Lock with a r/w lock???

   procedure Create (OA : out Simple_Obj_Adapter) is
   begin
      Create (OA.Lock);
   end Create;

   procedure Destroy (OA : in out Simple_Obj_Adapter) is
   begin
      Destroy (OA.Lock);
   end Destroy;

   procedure Export
     (OA  : in out Simple_Obj_Adapter;
      Obj : Objects.Servant_Access;
      Id  : out Object_Id) is
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
                  Object_Map_Entry'(Servant => Obj));
               New_Id := I;
               exit Map;
            end if;
         end loop Map;

         if New_Id > M'Last then
            Append (OA.Object_Map, Object_Map_Entry'
                    (Servant => Obj));
         end if;
         Leave (OA.Lock);

         Id := Object_Id (Index_To_Oid (New_Id - M'First + 1));
      end;
   end Export;

   procedure Unexport
     (OA : in out Simple_Obj_Adapter;
      Id : Object_Id) is
   begin
      raise Not_Implemented;
   end Unexport;

   function Find_Servant
     (OA  : Simple_Obj_Adapter;
      Id  : Object_Id)
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
     (OA : Simple_Obj_Adapter;
      Id : Object_Id;
      Servant : in out Servant_Access) is
   begin
      --  SOA: do nothing.
      Servant := null;
   end Release_Servant;

end Droopi.Obj_Adapters.Simple;
