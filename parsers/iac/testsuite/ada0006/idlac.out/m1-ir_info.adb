-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA.Object;
with CORBA.Repository_Root.IRObject.Helper;
with CORBA.Repository_Root.Container.Helper;
with PolyORB.CORBA_P.IR_Tools;
 use PolyORB.CORBA_P.IR_Tools;
with CORBA.Repository_Root;
 use CORBA.Repository_Root;

package body m1.IR_Info is


   Cached_IR_m1 :
     CORBA.Repository_Root.IRObject.Ref;

   function IR_m1
     return CORBA.Repository_Root.IRObject.Ref
   is
      Container_Ref : constant CORBA.Repository_Root.Container.Ref
        := CORBA.Repository_Root.Container.Helper.To_Ref (Get_IR_Root);
   begin
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_m1)
      then
         return Cached_IR_m1;
      end if;

      Cached_IR_m1 :=
        CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Lookup
         (Container_Ref,
          CORBA.To_CORBA_String ("m1")));
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_m1)
      then
         return Cached_IR_m1;
      end if;
      Cached_IR_m1
        := CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Create_Module
        (Container_Ref,
         CORBA.To_CORBA_String
           (Repository_Id),
         CORBA.To_CORBA_String
           ("m1"),
         CORBA.Repository_Root.To_CORBA_String
           ("1.0")));

      return Cached_IR_m1;
   end IR_m1;
   
   procedure Register_IR_Info is
   begin
      null;
      declare
         Dummy : CORBA.Object.Ref'Class
           := IR_m1;
         pragma Unreferenced (Dummy);
      begin
         null;
      end;
   end Register_IR_Info;

end m1.IR_Info;
