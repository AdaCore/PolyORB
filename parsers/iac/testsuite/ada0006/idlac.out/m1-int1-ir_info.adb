-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with m1.IR_Info;
with CORBA.Repository_Root.Repository;
with tin_IDL_File.IR_Info;
with CORBA.Repository_Root.InterfaceDef.Helper;
with CORBA.Object;
with CORBA.Repository_Root.IRObject.Helper;
with CORBA.Repository_Root.Container.Helper;
with PolyORB.CORBA_P.IR_Tools;
 use PolyORB.CORBA_P.IR_Tools;
with CORBA.Repository_Root;
 use CORBA.Repository_Root;

package body m1.int1.IR_Info is


   Cached_IR_attr1 :
     CORBA.Repository_Root.IRObject.Ref;

   function IR_attr1
     return CORBA.Repository_Root.IRObject.Ref
   is
      Container_Ref : constant CORBA.Repository_Root.Container.Ref
        := CORBA.Repository_Root.Container.Helper.To_Ref
        (m1.int1.IR_Info.IR_int1);
   begin
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_attr1)
      then
         return Cached_IR_attr1;
      end if;

      Cached_IR_attr1 :=
        CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Lookup
         (Container_Ref,
          CORBA.To_CORBA_String ("attr1")));
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_attr1)
      then
         return Cached_IR_attr1;
      end if;
      Cached_IR_attr1
        := CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.InterfaceDef.Create_Attribute
        (CORBA.Repository_Root.InterfaceDef.Helper.To_Ref
         (Container_Ref),
         id => CORBA.To_CORBA_String
           (attr1_Repository_Id),
         name => CORBA.To_CORBA_String
           ("attr1"),
         version => CORBA.Repository_Root.To_CORBA_String
           ("1.0"),
         IDL_type =>
         IDLType.Helper.To_Ref
         (tin_IDL_File.IR_Info.IR_New_Float),
         mode => ATTR_NORMAL));
      return Cached_IR_attr1;
   end IR_attr1;

   Cached_IR_bool1 :
     CORBA.Repository_Root.IRObject.Ref;

   function IR_bool1
     return CORBA.Repository_Root.IRObject.Ref
   is
      Container_Ref : constant CORBA.Repository_Root.Container.Ref
        := CORBA.Repository_Root.Container.Helper.To_Ref
        (m1.int1.IR_Info.IR_int1);
   begin
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_bool1)
      then
         return Cached_IR_bool1;
      end if;

      Cached_IR_bool1 :=
        CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Lookup
         (Container_Ref,
          CORBA.To_CORBA_String ("bool1")));
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_bool1)
      then
         return Cached_IR_bool1;
      end if;
      Cached_IR_bool1
        := CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.InterfaceDef.Create_Attribute
        (CORBA.Repository_Root.InterfaceDef.Helper.To_Ref
         (Container_Ref),
         id => CORBA.To_CORBA_String
           (bool1_Repository_Id),
         name => CORBA.To_CORBA_String
           ("bool1"),
         version => CORBA.Repository_Root.To_CORBA_String
           ("1.0"),
         IDL_type =>
         IDLType.Helper.To_Ref
         (CORBA.Repository_Root.Repository.Get_Primitive
           (Get_IR_Root, CORBA.Repository_Root.pk_boolean)),
         mode => ATTR_READONLY));
      return Cached_IR_bool1;
   end IR_bool1;

   Cached_IR_int1 :
     CORBA.Repository_Root.IRObject.Ref;

   function IR_int1
     return CORBA.Repository_Root.IRObject.Ref
   is
      Container_Ref : constant CORBA.Repository_Root.Container.Ref
        := CORBA.Repository_Root.Container.Helper.To_Ref
        (m1.IR_Info.IR_m1);
   begin
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_int1)
      then
         return Cached_IR_int1;
      end if;

      Cached_IR_int1 :=
        CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Lookup
         (Container_Ref,
          CORBA.To_CORBA_String ("int1")));
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_int1)
      then
         return Cached_IR_int1;
      end if;

      declare
         Base_Ifs : InterfaceDefSeq;
      begin
         Cached_IR_int1
           := CORBA.Repository_Root.IRObject.Helper.To_Ref
           (CORBA.Repository_Root.Container.Create_Interface
           (Container_Ref,
            CORBA.To_CORBA_String
              (Repository_Id),
            CORBA.To_CORBA_String
              ("int1"),
            To_CORBA_String ("1.0"),
            Base_Ifs,
            FALSE));

         return Cached_IR_int1;
      end;
   end IR_int1;
   
   procedure Register_IR_Info is
   begin
      null;
      declare
         Dummy : CORBA.Object.Ref'Class
           := IR_attr1;
         pragma Unreferenced (Dummy);
      begin
         null;
      end;
      declare
         Dummy : CORBA.Object.Ref'Class
           := IR_bool1;
         pragma Unreferenced (Dummy);
      begin
         null;
      end;
      declare
         Dummy : CORBA.Object.Ref'Class
           := IR_int1;
         pragma Unreferenced (Dummy);
      begin
         null;
      end;
   end Register_IR_Info;

end m1.int1.IR_Info;
