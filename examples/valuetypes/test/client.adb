with Va;
with Vb;
with Vc;
with Va.Value_Impl;
with Vc.Value_Impl;
with Corba.Impl;
with Vb.Helper;

with Ada.Text_Io; use Ada.Text_Io;

procedure Client is
   Obja : Va.Value_Impl.Object_Ptr
     := new Va.Value_Impl.Object;
   Objc : Vc.Value_Impl.Object_Ptr
     := new Vc.Value_Impl.Object;
   Refa : Va.Value_Ref;
   Refb : Vb.Abstract_Value_Ref;
   Refc : Vc.Value_Ref;

begin

   Va.Set (RefA, CORBA.Impl.Object_Ptr (ObjA));
   Vc.Set (Refc, CORBA.Impl.Object_Ptr (Objc));
   Va.FooA (RefA, 0);
   Vc.FooA (Refc, 0);
   Vc.Fooc (Refc);

   Refb := Vb.Helper.To_Abstract_Value_Ref (Refc);
   Vb.Foob (Refb);

end Client;
