-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA.Repository_Root.InterfaceDef.Helper;
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

package body i1.IR_Info is


   Cached_IR_New_Float :
     CORBA.Repository_Root.IRObject.Ref;

   function IR_New_Float
     return CORBA.Repository_Root.IRObject.Ref
   is
      Container_Ref : constant CORBA.Repository_Root.Container.Ref
        := CORBA.Repository_Root.Container.Helper.To_Ref
        (i1.IR_Info.IR_i1);
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

   Cached_IR_Tab_Float :
     CORBA.Repository_Root.IRObject.Ref;

   function IR_Tab_Float
     return CORBA.Repository_Root.IRObject.Ref
   is
      Container_Ref : constant CORBA.Repository_Root.Container.Ref
        := CORBA.Repository_Root.Container.Helper.To_Ref
        (i1.IR_Info.IR_i1);
   begin
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_Tab_Float)
      then
         return Cached_IR_Tab_Float;
      end if;

      Cached_IR_Tab_Float :=
        CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Lookup
         (Container_Ref,
          CORBA.To_CORBA_String ("Tab_Float")));
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_Tab_Float)
      then
         return Cached_IR_Tab_Float;
      end if;

      Cached_IR_Tab_Float
        := CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Create_Alias
         (Container_Ref,
         id => CORBA.To_CORBA_String
           (Tab_Float_Repository_Id),
         name => CORBA.To_CORBA_String
           ("Tab_Float"),
         version => CORBA.Repository_Root.To_CORBA_String
           ("1.0"),
         original_type => IDLType.Convert_Forward.To_Forward
           (CORBA.Repository_Root.IDLType.Helper.To_Ref
              (Repository.Create_Array
               (Get_IR_Root,
                   length => 10,
                   element_type => IDLType.Helper.To_Ref
                     (CORBA.Repository_Root.Repository.Get_Primitive
                       (Get_IR_Root, CORBA.Repository_Root.pk_float))))
            )));
      return Cached_IR_Tab_Float;
   end IR_Tab_Float;

   Cached_IR_val1 :
     CORBA.Repository_Root.IRObject.Ref;

   function IR_val1
     return CORBA.Repository_Root.IRObject.Ref
   is
      Container_Ref : constant CORBA.Repository_Root.Container.Ref
        := CORBA.Repository_Root.Container.Helper.To_Ref
        (i1.IR_Info.IR_i1);
   begin
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_val1)
      then
         return Cached_IR_val1;
      end if;

      Cached_IR_val1 :=
        CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Lookup
         (Container_Ref,
          CORBA.To_CORBA_String ("val1")));
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_val1)
      then
         return Cached_IR_val1;
      end if;
      Cached_IR_val1
        := CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.InterfaceDef.Create_Attribute
        (CORBA.Repository_Root.InterfaceDef.Helper.To_Ref
         (Container_Ref),
         id => CORBA.To_CORBA_String
           (val1_Repository_Id),
         name => CORBA.To_CORBA_String
           ("val1"),
         version => CORBA.Repository_Root.To_CORBA_String
           ("1.0"),
         IDL_type =>
         IDLType.Helper.To_Ref
         (CORBA.Repository_Root.Repository.Get_Primitive
           (Get_IR_Root, CORBA.Repository_Root.pk_float)),
         mode => ATTR_NORMAL));
      return Cached_IR_val1;
   end IR_val1;

   Cached_IR_val2 :
     CORBA.Repository_Root.IRObject.Ref;

   function IR_val2
     return CORBA.Repository_Root.IRObject.Ref
   is
      Container_Ref : constant CORBA.Repository_Root.Container.Ref
        := CORBA.Repository_Root.Container.Helper.To_Ref
        (i1.IR_Info.IR_i1);
   begin
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_val2)
      then
         return Cached_IR_val2;
      end if;

      Cached_IR_val2 :=
        CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Lookup
         (Container_Ref,
          CORBA.To_CORBA_String ("val2")));
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_val2)
      then
         return Cached_IR_val2;
      end if;
      Cached_IR_val2
        := CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.InterfaceDef.Create_Attribute
        (CORBA.Repository_Root.InterfaceDef.Helper.To_Ref
         (Container_Ref),
         id => CORBA.To_CORBA_String
           (val2_Repository_Id),
         name => CORBA.To_CORBA_String
           ("val2"),
         version => CORBA.Repository_Root.To_CORBA_String
           ("1.0"),
         IDL_type =>
         IDLType.Helper.To_Ref
         (CORBA.Repository_Root.Repository.Get_Primitive
           (Get_IR_Root, CORBA.Repository_Root.pk_float)),
         mode => ATTR_NORMAL));
      return Cached_IR_val2;
   end IR_val2;

   Cached_IR_tab_val :
     CORBA.Repository_Root.IRObject.Ref;

   function IR_tab_val
     return CORBA.Repository_Root.IRObject.Ref
   is
      Container_Ref : constant CORBA.Repository_Root.Container.Ref
        := CORBA.Repository_Root.Container.Helper.To_Ref
        (i1.IR_Info.IR_i1);
   begin
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_tab_val)
      then
         return Cached_IR_tab_val;
      end if;

      Cached_IR_tab_val :=
        CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Lookup
         (Container_Ref,
          CORBA.To_CORBA_String ("tab_val")));
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_tab_val)
      then
         return Cached_IR_tab_val;
      end if;
      Cached_IR_tab_val
        := CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.InterfaceDef.Create_Attribute
        (CORBA.Repository_Root.InterfaceDef.Helper.To_Ref
         (Container_Ref),
         id => CORBA.To_CORBA_String
           (tab_val_Repository_Id),
         name => CORBA.To_CORBA_String
           ("tab_val"),
         version => CORBA.Repository_Root.To_CORBA_String
           ("1.0"),
         IDL_type =>
         IDLType.Helper.To_Ref
         (CORBA.Repository_Root.Repository.Get_Primitive
           (Get_IR_Root, CORBA.Repository_Root.pk_float)),
         mode => ATTR_NORMAL));
      return Cached_IR_tab_val;
   end IR_tab_val;

   Cached_IR_tab :
     CORBA.Repository_Root.IRObject.Ref;

   function IR_tab
     return CORBA.Repository_Root.IRObject.Ref
   is
      Container_Ref : constant CORBA.Repository_Root.Container.Ref
        := CORBA.Repository_Root.Container.Helper.To_Ref
        (i1.IR_Info.IR_i1);
   begin
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_tab)
      then
         return Cached_IR_tab;
      end if;

      Cached_IR_tab :=
        CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Lookup
         (Container_Ref,
          CORBA.To_CORBA_String ("tab")));
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_tab)
      then
         return Cached_IR_tab;
      end if;
      Cached_IR_tab
        := CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.InterfaceDef.Create_Attribute
        (CORBA.Repository_Root.InterfaceDef.Helper.To_Ref
         (Container_Ref),
         id => CORBA.To_CORBA_String
           (tab_Repository_Id),
         name => CORBA.To_CORBA_String
           ("tab"),
         version => CORBA.Repository_Root.To_CORBA_String
           ("1.0"),
         IDL_type =>
         IDLType.Helper.To_Ref
         (i1.IR_Info.IR_Tab_Float),
         mode => ATTR_NORMAL));
      return Cached_IR_tab;
   end IR_tab;

   Cached_IR_i1 :
     CORBA.Repository_Root.IRObject.Ref;

   function IR_i1
     return CORBA.Repository_Root.IRObject.Ref
   is
      Container_Ref : constant CORBA.Repository_Root.Container.Ref
        := CORBA.Repository_Root.Container.Helper.To_Ref (Get_IR_Root);
   begin
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_i1)
      then
         return Cached_IR_i1;
      end if;

      Cached_IR_i1 :=
        CORBA.Repository_Root.IRObject.Helper.To_Ref
        (CORBA.Repository_Root.Container.Lookup
         (Container_Ref,
          CORBA.To_CORBA_String ("i1")));
      if not CORBA.Repository_Root.IRObject.Is_Nil
        (Cached_IR_i1)
      then
         return Cached_IR_i1;
      end if;

      declare
         Base_Ifs : InterfaceDefSeq;
      begin
         Cached_IR_i1
           := CORBA.Repository_Root.IRObject.Helper.To_Ref
           (CORBA.Repository_Root.Container.Create_Interface
           (Container_Ref,
            CORBA.To_CORBA_String
              (Repository_Id),
            CORBA.To_CORBA_String
              ("i1"),
            To_CORBA_String ("1.0"),
            Base_Ifs,
            FALSE));

         return Cached_IR_i1;
      end;
   end IR_i1;
   
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
           := IR_Tab_Float;
         pragma Unreferenced (Dummy);
      begin
         null;
      end;
      declare
         Dummy : CORBA.Object.Ref'Class
           := IR_val1;
         pragma Unreferenced (Dummy);
      begin
         null;
      end;
      declare
         Dummy : CORBA.Object.Ref'Class
           := IR_val2;
         pragma Unreferenced (Dummy);
      begin
         null;
      end;
      declare
         Dummy : CORBA.Object.Ref'Class
           := IR_tab_val;
         pragma Unreferenced (Dummy);
      begin
         null;
      end;
      declare
         Dummy : CORBA.Object.Ref'Class
           := IR_tab;
         pragma Unreferenced (Dummy);
      begin
         null;
      end;
      declare
         Dummy : CORBA.Object.Ref'Class
           := IR_i1;
         pragma Unreferenced (Dummy);
      begin
         null;
      end;
   end Register_IR_Info;

end i1.IR_Info;
