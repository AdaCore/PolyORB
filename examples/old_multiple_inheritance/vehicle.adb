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

with Text_IO ;

with Ada.Tags, Ada.exceptions ;

with Corba.Object ; use Corba.Object ;

package body Vehicle is

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
      Text_IO.Put_Line("vehicle: from: " & Ada.Tags.External_Tag(The_Ref'Tag)) ;
      Text_IO.Put_Line("vehicle: dyn: "&Ada.Tags.External_Tag(Dynamic_Ref'Tag)) ;
      AdaBroker_Cast_To_Parent(Dynamic_Ref,Result) ;
      return Result ;
   end ;




   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

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
         Tmp_Result : Corba.Object.Ref ;
      begin
         Tmp_Result := Corba.Object.Ref(Real_Ref) ;
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

