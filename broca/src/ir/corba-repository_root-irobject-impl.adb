----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with Ada.Unchecked_Deallocation;
with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.IRObject.Skel;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.Container.Impl;
with Broca.Debug;
with Broca.Exceptions;

package body CORBA.Repository_Root.IRObject.Impl is

   -----------
   -- Debug --
   -----------

   Flag : constant Natural
     := Broca.Debug.Is_Active ("irobject.impl");
   procedure O is new Broca.Debug.Output (Flag);

   Flag2 : constant Natural
     := Broca.Debug.Is_Active ("irobject.impl_method_trace");
   procedure O2 is new Broca.Debug.Output (Flag2);


   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : Corba.Repository_Root.DefinitionKind) is
   begin
      pragma Debug (O2 ("init enter"));
      Self.Def_Kind := Def_Kind;
      Self.Real_Object := Real_Object;
      pragma Debug (O2 ("init  end"));
   end Init;



   -----------------------
   --  Get_Real_Object  --
   -----------------------
   function Get_Real_Object (Self : access Object)
     return Object_Ptr is
   begin
        return Self.Real_Object;
   end Get_Real_Object;



   ------------------------------
   --  generated automatically --
   ------------------------------

   function get_def_kind
     (Self : access Object)
     return CORBA.Repository_Root.DefinitionKind
   is
   begin
      return Self.Def_Kind;
   end get_def_kind;


   -------------------------
   --  Remove_Object_Ptr  --
   -------------------------
   type Simple_Object_Ptr is access all Object;

   procedure Simple_Unchecked_Deallocation is
     new Ada.Unchecked_Deallocation
     (Object, Simple_Object_Ptr);

   procedure destroy
     (Self : access Object) is
      Self2 : Simple_Object_Ptr := Simple_Object_Ptr (Self);
   begin
      pragma Debug (O2 ("IRObject destroy : enter"));
      --  is overriden in each necessary defs
      case Self.Def_Kind is
         when
           --  You are not allowed to destroy rarepository aree a primitive
           Dk_Repository |
           Dk_Primitive =>
            Broca.Exceptions.Raise_Bad_Inv_Order (Minor => 2);
         when others =>
            --  dispatching call
            --  Destroy (Object_Ptr (Self));

            --  Implemented for purpose of a DEMO...

            --  FIXME  memory leak, should be dispatched
            --  remove the contained from the previous container
            declare
               Cont : Contained.Impl.Object_Ptr
                 := Contained.Impl.To_Contained (Get_Real_Object (Self));
            begin
               Container.Impl.Delete_From_Contents
                 (Container.Impl.To_Object
                  (Contained.Impl.Get_Defined_In (Cont)),
                  Cont);
            end;
      end case;
   end destroy;

end CORBA.Repository_Root.IRObject.Impl;





