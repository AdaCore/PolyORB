with Broca.Refs; use Broca.Refs;

package body CORBA.Forward is
   package body Convert is
      --  FIXME: minimal implementation.
      function From_Forward (The_Forward : in Ref)  return Ref_Type is
         Res : Ref_Type;
      begin
         Set (Res, Get (The_Forward));
         return Res;
      end From_Forward;

      function To_Forward (The_Ref : in Ref_Type) return Ref is
         Res : Ref;
      begin
         Broca.Refs.Set (Broca.Refs.Ref (Res), Get (The_Ref));
         return Res;
      end To_Forward;
   end Convert;
end CORBA.Forward;
