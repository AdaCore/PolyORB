with Broca.Refs;
with CORBA.Object;

generic
package CORBA.Forward is

   type Ref is new CORBA.Object.Ref with null record;

   generic
      type Ref_Type is new CORBA.Object.Ref with private;
   package Convert is
      function From_Forward (The_Forward : in Ref)  return Ref_Type;
      function To_Forward   (The_Ref : in Ref_Type) return Ref;
   end Convert;
end CORBA.Forward;



