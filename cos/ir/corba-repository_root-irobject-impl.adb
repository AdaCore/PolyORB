----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.Container.Impl;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.CORBA_P.Exceptions;

with CORBA.Repository_Root.IRObject.Skel;
pragma Warnings (Off, CORBA.Repository_Root.IRObject.Skel);

package body CORBA.Repository_Root.IRObject.Impl is

   -----------
   -- Debug --
   -----------

   use PolyORB.Log;

--    package L is new PolyORB.Log.Facility_Log ("irobject.impl");
--    procedure O (Message : in Standard.String; Level : Log_Level := Debug)
--      renames L.Output;

   package L2 is new PolyORB.Log.Facility_Log ("irobject.impl_method_trace");
   procedure O2 (Message : in Standard.String; Level : Log_Level := Debug)
     renames L2.Output;


   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind) is
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

   procedure destroy
     (Self : access Object) is
   begin
      pragma Debug (O2 ("IRObject destroy : enter"));
      --  is overriden in each necessary defs
      case Self.Def_Kind is
         when
            dk_Repository |
            dk_Primitive  =>
            --  You are not allowed to destroy rarepository aree a primitive
            PolyORB.CORBA_P.Exceptions.Raise_Bad_Inv_Order (Minor => 2);
         when others =>
            --  dispatching call
            --  Destroy (Object_Ptr (Self));

            --  Implemented for purpose of a DEMO...

            --  FIXME  memory leak, should be dispatched
            --  remove the contained from the previous container
            declare
               Cont : constant Contained.Impl.Object_Ptr
                 := Contained.Impl.To_Contained (Get_Real_Object (Self));
            begin
               Container.Impl.Delete_From_Contents
                 (Container.Impl.To_Object
                  (Contained.Impl.get_defined_in (Cont)),
                  Cont);
            end;
      end case;
   end destroy;

end CORBA.Repository_Root.IRObject.Impl;





