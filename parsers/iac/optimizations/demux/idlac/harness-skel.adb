-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with PolyORB.Utils.Strings;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);
with CORBA.ORB;
with CORBA.NVList;
with CORBA.ServerRequest;
with Harness.Impl;
with CORBA;
pragma Elaborate_All (CORBA);
with PortableServer;
pragma Elaborate_All (PortableServer);
with PolyORB.CORBA_P.Exceptions;

package body Harness.Skel is

   --  Skeleton subprograms

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean;

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean is
   begin
      return Obj.all in Harness.Impl.Object'Class;
   end Servant_Is_A;

   procedure Invoke
     (Self : PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_ptr)
   is
      Operation : constant Standard.String
         := CORBA.To_Standard_String
              (CORBA.ServerRequest.Operation
               (Request.all));
      Arg_List_Ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List (0, Arg_List_Ü);
      if Operation = "_is_a" then
         declare
            Type_Id            : CORBA.String;
            Arg_Name_Ü_Type_Id : constant CORBA.Identifier
            := CORBA.To_CORBA_String ("Type_Id");
            Argument_Ü_Type_Id : CORBA.Any := CORBA.To_Any (Type_Id);
            
            Result_Ü           : CORBA.Boolean;
         begin
            CORBA.NVList.Add_Item
            (Arg_List_Ü,
            Arg_Name_Ü_Type_Id,
            Argument_Ü_Type_Id,
            CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               Type_Id :=
                 CORBA.From_Any (Argument_Ü_Type_Id);

               --  Call implementation
               Result_Ü := Harness.Is_A
                 (CORBA.To_Standard_String (Type_Id));
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
            (Request,
            CORBA.To_Any (
            Result_Ü));
            return;
         end;

      elsif Operation = "echoULong1" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong1
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong2" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong2
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong3" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong3
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong4" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong4
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong5" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong5
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong6" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong6
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong7" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong7
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong8" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong8
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong9" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong9
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong10" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong10
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong11" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong11
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong12" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong12
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong13" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong13
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong14" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong14
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong15" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong15
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong16" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong16
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong17" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong17
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong18" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong18
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong19" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong19
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "echoULong20" then

         declare
            arg            : CORBA.Unsigned_Long;
            Arg_Name_Ü_arg : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("arg");
            Argument_Ü_arg : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long);

            Result_Ü       : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_arg,
               Argument_Ü_arg,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               arg :=
                 CORBA.From_Any (Argument_Ü_arg);

               --  Call implementation
               Result_Ü := Harness.Impl.echoULong20
                 (Harness.Impl.Object'Class (Self.all)'Access,
                  arg);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (
               Result_Ü));
            return;
         end;

      else
         CORBA.Raise_Bad_Operation (CORBA.Default_Sys_Member);
      end if;
   exception
      when E : others =>
         begin
            CORBA.ServerRequest.Set_Exception
              (Request,
               CORBA.Internals.To_CORBA_Any (PolyORB.CORBA_P.Exceptions.System_Exception_To_Any (E)));
            return;
         end;
   end Invoke;
   procedure Deferred_Initialization is
   begin
      null;
      PortableServer.Register_Skeleton
        (CORBA.To_CORBA_String (Harness.Repository_Id),
         Servant_Is_A'Access,
         Invoke'Access);
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"Harness.Skel",
          Conflicts => Empty,
          Depends   =>
                  Empty
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end Harness.Skel;
