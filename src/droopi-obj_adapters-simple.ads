--  Object adapters: entities that manage the association
--  of references with servants.

--  $Id$

with Sequences.Unbounded;

with Droopi.Soft_Links;

package Droopi.Obj_Adapters.Simple is

   type Simple_Obj_Adapter is new Obj_Adapter with private;

   procedure Create (OA : out Simple_Obj_Adapter);

   procedure Destroy (OA : in out Simple_Obj_Adapter);

   procedure Export
     (OA  : in out Simple_Obj_Adapter;
      Obj : Objects.Servant_Access;
      Id  : out Object_Id);

   procedure Unexport
     (OA : in out Simple_Obj_Adapter;
      Id : Object_Id);

   function Resolve
     (OA  : Simple_Obj_Adapter;
      Id : Object_Id)
     return Servant_Access;

private

   type Object_Map_Entry is record
      --  The Object_Id is simply the position of the
      --  object within the object map.
      Servant : Servant_Access;
      --  May be null (empty entries).
   end record;

   package Object_Map_Entry_Seqs is new Sequences.Unbounded
     (Object_Map_Entry);
   subtype Object_Map_Entry_Seq is Object_Map_Entry_Seqs.Sequence;

   type Simple_Obj_Adapter is new Obj_Adapter with record
      Object_Map : Object_Map_Entry_Seq;
      Lock : Soft_Links.Adv_Mutex_Access;
   end record;

end Droopi.Obj_Adapters.Simple;
