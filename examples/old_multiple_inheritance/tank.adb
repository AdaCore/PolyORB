----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the Vehicle object                                         ----
----                                                                    ----
----                package body Weapon                                 ----
----                                                                    ----
----     author : Sebastien Ponce, Fabien Azavant                       ----
----                                                                    ----
----------------------------------------------------------------------------


-- TO DO ::::::
-- remove useless comments to show what has to be generated
-- add tests to check that the Weapon_ref is valid


with Text_IO ;

with Ada.Unchecked_Deallocation ;
with Ada.Exceptions, Ada.Tags ;
with Ada.Unchecked_Conversion ;

with Corba.Object ; use Corba.Object ;

package body Tank is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   -- To_Ref
   ---------
   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref is
      Dynamic_Ref : Corba.Object.Ref'Class
        := Get_Dynamic_Ref(The_Ref) ;
      Result : Ref ;
   begin
      Text_IO.Put_Line("Tank.To_Ref(" & Ada.Tags.External_Tag(The_Ref'Tag)
                       & " with dyn_ref: "
                       & Ada.Tags.External_Tag(Dynamic_Ref'Tag)
                       & ") return Tank.Ref");
      AdaBroker_Cast_To_Parent(Dynamic_Ref,Result) ;
      return Result ;
   end ;




   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   --  Get_Dynamic_Ref
   ----------------------
    function Get_Dynamic_Ref(Self: in Ref) return Corba.Object.Ref'Class is
    begin
       Text_IO.Put_Line("Tank.Get_Dynamic_Ref(Tank.Ref) return ??") ;
       return Self ;
    end ;

    -- AdaBroker_Cast_To_Parent
   ---------------------------
   procedure AdaBroker_Cast_To_Parent(Real_Ref: in Ref;
                                      Result: out Corba.Object.Ref'Class) is
   begin

      -- I am the result !
      if Result in Ref then
         declare
            Tmp_Result : Corba.Object.Ref'Class := Real_Ref ;
         begin
            Result := Tmp_Result ;
            return ;
         end ;
      end if ;

      --try my first parent
      declare
         Tmp_Result : Vehicle.Ref ;
      begin
         Tmp_Result := Vehicle.Ref(Real_Ref) ;
         Vehicle.AdaBroker_Cast_To_Parent(Tmp_Result, Result) ;
         return ;
      exception
         when Constraint_Error => null ;
      end ;

      -- try my second parent
      declare
         Tmp_Result : Weapon.Ref ;
      begin
         Tmp_Result := Weapon.Ref(Real_Ref.AdaBroker_Weapon) ;
         Weapon.AdaBroker_Cast_To_Parent(Tmp_Result, Result) ;
         return ;
      exception
         when Constraint_Error => null ;
      end ;

      Ada.Exceptions.Raise_Exception(Constraint_Error'Identity,
                                     "Tank.To_Ref :"
                                     & Corba.CRLF
                                     & "  Cannot cast Tank.Ref"
                                     & Corba.CRLF
                                     & "  into "
                                     & Ada.Tags.External_Tag(Result'tag)) ;
   end ;


   -- Initialize
   -------------
   procedure Initialize(Object : in out Ref) is
      type Ptr is access all Ref;
      function To_Ref_Ptr is new Ada.Unchecked_Conversion (Ptr, Ref_Ptr);
   begin
      Text_Io.Put_Line("   < Initializing Tank.Ref !! >") ;
      Object.AdaBroker_Weapon.Dynamic_Ref := To_Ref_Ptr(Object'Access) ;
   end ;


   -- Adjust
   ---------
   procedure Adjust(Object : in out Weapon_Ref) is
   begin
      Object.Must_Be_Freed := False ;
   end ;



   --------------------------------------------------
   ----     inheritance from Weapon              ----
   --------------------------------------------------

   -- Get_Dynamic_Ref
   ------------------
   function Get_Dynamic_Ref(Self: in Weapon_Ref) return Corba.Object.Ref'Class is
      Tmp : Ref  ;
      Tmp_Ptr : Ref_Ptr ;
   begin
      Text_IO.Put_Line("Tank.Get_Dynamic_Ref(Tank.Weapon_Ref) return ??:start") ;
      Tmp_ptr := Self.Dynamic_Ref ;
      Text_IO.Put_Line("Tank.Get_Dynamic_Ref(Tank.Weapon_Ref) return ??"
                       &":got the Ref_Ptr") ;
      if Tmp_Ptr = null then Text_Io.Put_Line("NULL") ; end if ;
      Tmp := Tmp_Ptr.all ;
      Text_IO.Put_Line("Tank.Get_Dynamic_Ref(Tank.Weapon_Ref) return ??"
                       &":got the Ref") ;
      return Get_Dynamic_Ref(tmp) ;
   end ;


   -- To_Ref
   ---------
   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Weapon_Ref is
      Tmp : Ref_Ptr ;
   begin
      Text_IO.Put_Line("Tank.To_Ref(" & Ada.Tags.External_Tag(The_Ref'Tag)
                       & ") return Tank.Weapon_Ref") ;
      Tmp := new Ref'(To_Ref(The_Ref)) ;
      Text_IO.Put_Line("******* Created Tank.Ref *******") ;
      Tmp.AdaBroker_Weapon.Must_Be_Freed := True ;
      return Tmp.AdaBroker_Weapon ;
   end ;



   -- an invalid object of this type
   -- it points to nohing and is used in the Finalize procedure
   Invalid_Weapon_Ref : Weapon_Ref ;

   -- Finalize
   -----------
   procedure Finalize(Object : in out Weapon_Ref) is
      procedure Free is new Ada.Unchecked_Deallocation(Ref, Ref_Ptr) ;
   begin
      Text_Io.Put_Line("   < Finalizing Tank.Weapon_Ref !! >") ;
      if Object.Must_Be_Freed then
         Object.Must_Be_Freed := False ;
         Object.Dynamic_Ref.all.AdaBroker_Weapon := Invalid_Weapon_Ref ;
         Text_IO.Put_Line("******* Freeing Tank.ref") ;
         Free(Object.Dynamic_Ref) ;
      end if ;
   end ;



End Tank ;



