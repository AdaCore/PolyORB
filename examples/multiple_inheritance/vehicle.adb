----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the Vehicle object                                         ----
----                                                                    ----
----                package body Vehicle                                ----
----                                                                    ----
----     author : Sebastien Ponce, Fabien Azavant                       ----
----                                                                    ----
----------------------------------------------------------------------------

with Ada.Tags, Ada.exceptions ;

with Corba.Object ; use Corba.Object ;

package body Vehicle is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   -- To_Ref
   ---------
   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref is
      Dynamic_Object : Corba.Object.Ref'Class
        := Corba.Object.Get_Dynamic_Object(The_Ref) ;
      Result : Ref ;
   begin
      AdaBroker_Cast_To_Parent(Dynamic_Object,Result) ;
      return Result ;
   end ;




   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   -- AdaBroker_Cast_To_Parent
   ---------------------------
   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object.Ref'Class) is
   begin
      -- I am the result !
      if Result in Ref then
         declare
            Tmp_Result : Corba.Object.Ref'Class := Real_Object ;
         begin
            Result := Tmp_Result ;
            return ;
         end ;
      end if ;

      --try my first parent
      declare
         Tmp_Result : Corba.Object.Ref ;
      begin
         Tmp_Result := Corba.Object.Ref(Real_Object) ;
         Corba.Object.AdaBroker_Cast_To_Parent(Tmp_Result, Result) ;
         return ;
      exception
         when Constraint_Error => null ;
      end ;

      Ada.Exceptions.Raise_Exception(Constraint_Error'Identity,
                                     "Vehicle.To_Ref :"
                                     & Corba.CRLF
                                     & "  Cannot cast Vehicle.Ref"
                                     & Corba.CRLF
                                     & "  into "
                                     & Ada.Tags.External_Tag(Result'tag)) ;
   end ;



End Vehicle ;

