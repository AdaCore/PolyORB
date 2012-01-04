------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 CORBA.REPOSITORY_ROOT.OPERATIONDEF.IMPL                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Style_Checks (Off);

with CORBA.Repository_Root.Helper;
with CORBA.Repository_Root.ExceptionDef.Impl;
with CORBA.Repository_Root.OperationDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.OperationDef.Skel);

package body CORBA.Repository_Root.OperationDef.Impl is

   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   Result_Def : CORBA.Repository_Root.IDLType.Ref;
                   Params : CORBA.Repository_Root.ParDescriptionSeq;
                   Mode : CORBA.Repository_Root.OperationMode;
                   Contexts : CORBA.Repository_Root.ContextIdSeq;
                   Exceptions : CORBA.Repository_Root.ExceptionDefSeq) is
   begin
      Contained.Impl.Init (Contained.Impl.Object_Ptr(Self),
                           Real_Object,
                           Def_Kind,
                           Id,
                           Name,
                           Version,
                           Defined_In);
      Self.Result_Def := Result_Def;
      Self.Params := Params;
      Self.Mode := Mode;
      Self.Contexts := Contexts;
      Self.Exceptions := Exceptions;
   end Init;

   function get_result
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
   begin
      return IDLType.Get_Type (get_result_def (Self));
   end get_result;

   function get_result_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref
   is
   begin
      return Self.Result_Def;
   end get_result_def;

   procedure set_result_def
     (Self : access Object;
      To : CORBA.Repository_Root.IDLType.Ref) is
   begin
      Self.Result_Def := To;
   end set_result_def;

   function get_params
     (Self : access Object)
     return CORBA.Repository_Root.ParDescriptionSeq
   is
   begin
      return Self.Params;
   end get_params;

   procedure set_params
     (Self : access Object;
      To : CORBA.Repository_Root.ParDescriptionSeq) is
   begin
      Self.Params := To;
   end set_params;

   function get_mode
     (Self : access Object)
     return CORBA.Repository_Root.OperationMode
   is
   begin
      return Self.Mode;
   end get_mode;

   procedure set_mode
     (Self : access Object;
      To : CORBA.Repository_Root.OperationMode) is
   begin
      Self.Mode := To;
   end set_mode;

   function get_contexts
     (Self : access Object)
     return CORBA.Repository_Root.ContextIdSeq
   is
   begin
      return Self.Contexts;
   end get_contexts;

   procedure set_contexts
     (Self : access Object;
      To : CORBA.Repository_Root.ContextIdSeq) is
   begin
      Self.Contexts := To;
   end set_contexts;

   function get_exceptions
     (Self : access Object)
     return CORBA.Repository_Root.ExceptionDefSeq
   is
   begin
      return Self.Exceptions;
   end get_exceptions;

   procedure set_exceptions
     (Self : access Object;
      To : CORBA.Repository_Root.ExceptionDefSeq) is
   begin
      Self.Exceptions := To;
   end set_exceptions;

   ----------------
   --  Describe  --
   ----------------
   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description
     is
      Result : CORBA.Repository_Root.Contained.Description;
      Desc : CORBA.Repository_Root.OperationDescription;
   begin
      Desc := (Name => Get_Name (Self),
               Id => Get_Id (Self),
               Defined_In => Get_Defined_In (Self),
               Version => Get_Version (Self),
               Result => Get_Result (Self),
               Mode => Self.Mode,
               Contexts => Self.Contexts,
               Parameters => Self.Params,
               Exceptions => ExceptionDef.Impl.Get_ExcDescriptionSeq
               (Self.Exceptions));
      Result := (Kind => Get_Def_Kind (Self),
                 Value => CORBA.Repository_Root.Helper.To_Any (Desc));
      return Result;
   end describe;

end CORBA.Repository_Root.OperationDef.Impl;
