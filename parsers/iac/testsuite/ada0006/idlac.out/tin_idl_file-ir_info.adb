-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA.Repository_Root.Repository;
with CORBA.Repository_Root.IDLType.Helper;
with CORBA.Repository_Root.IDLType;
with CORBA.Object;
with CORBA.Repository_Root.Container.Helper;
with CORBA.Repository_Root.IRObject.Helper;
with PolyORB.CORBA_P.IR_Tools;
 use PolyORB.CORBA_P.IR_Tools;
with CORBA.Repository_Root;
 use CORBA.Repository_Root;

package body tin_IDL_File.IR_Info is


   Cached_IR_New_Float :
     CORBA.Repository_Root.IRObject.Ref;

   function IR_New_Float
     return CORBA.Repository_Root.IRObject.Ref
   is
      Container_Ref : constant CORBA.Repository_Root.Container.Ref
        := CORBA.Repository_Root.Container.Helper.To_Ref (Get_IR_Root);
   begin
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_New_Float)
      then
         return Cached_IR_New_Float;
      end if;

      Cached_IR_New_Float :=
        CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Lookup
         (Container_Ref,
          CORBA.To_CORBA_String ("New_Float")));
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_New_Float)
      then
         return Cached_IR_New_Float;
      end if;

      Cached_IR_New_Float
        := CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Create_Alias
         (Container_Ref,
         id => CORBA.To_CORBA_String
           (New_Float_Repository_Id),
         name => CORBA.To_CORBA_String
           ("New_Float"),
         version => CORBA.Repository_Root.To_CORBA_String
           ("1.0"),
         original_type => IDLType.Convert_Forward.To_Forward
           (IDLType.Helper.To_Ref
            (CORBA.Repository_Root.Repository.Get_Primitive
              (Get_IR_Root, CORBA.Repository_Root.pk_float)))));
      return Cached_IR_New_Float;
   end IR_New_Float;

   Cached_IR_New_Boolean :
     CORBA.Repository_Root.IRObject.Ref;

   function IR_New_Boolean
     return CORBA.Repository_Root.IRObject.Ref
   is
      Container_Ref : constant CORBA.Repository_Root.Container.Ref
        := CORBA.Repository_Root.Container.Helper.To_Ref (Get_IR_Root);
   begin
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_New_Boolean)
      then
         return Cached_IR_New_Boolean;
      end if;

      Cached_IR_New_Boolean :=
        CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Lookup
         (Container_Ref,
          CORBA.To_CORBA_String ("New_Boolean")));
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_New_Boolean)
      then
         return Cached_IR_New_Boolean;
      end if;

      Cached_IR_New_Boolean
        := CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Create_Alias
         (Container_Ref,
         id => CORBA.To_CORBA_String
           (New_Boolean_Repository_Id),
         name => CORBA.To_CORBA_String
           ("New_Boolean"),
         version => CORBA.Repository_Root.To_CORBA_String
           ("1.0"),
         original_type => IDLType.Convert_Forward.To_Forward
           (IDLType.Helper.To_Ref
            (CORBA.Repository_Root.Repository.Get_Primitive
              (Get_IR_Root, CORBA.Repository_Root.pk_boolean)))));
      return Cached_IR_New_Boolean;
   end IR_New_Boolean;
   
   procedure Register_IR_Info is
   begin
      null;
      declare
         Dummy : CORBA.Object.Ref'Class
           := IR_New_Float;
         pragma Unreferenced (Dummy);
      begin
         null;
      end;
      declare
         Dummy : CORBA.Object.Ref'Class
           := IR_New_Boolean;
         pragma Unreferenced (Dummy);
      begin
         null;
      end;
   end Register_IR_Info;

end tin_IDL_File.IR_Info;
