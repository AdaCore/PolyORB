------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--          G L A D E . N A M I N G . I M P L E M E N T A T I O N           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Streams;                use Ada.Streams;
with GLADE.Naming.Interface;     use GLADE.Naming.Interface;
with GLADE.Objects;              use GLADE.Objects;
with System.IO;

package body GLADE.Naming.Implementation is

   --  Local access type for local allocation

   type Binding_Iterator_Ptr is access all Binding_Iterator;

   procedure Append_BO_To_BOL
     (BOL : in out Bound_Objects_List;
      N   : in Name;
      Ctx : in Interface.Naming_Context_Ref;
      Obj : in Objects.Object_Ref);
   --  Append bound object to bound objects list. Depending whether Ctx or
   --  Obj is null, initialize bound object discriminant. Do not check
   --  whether this binding already exists in the list.

   function Create
     (NC  : Name_Component;
      Ctx : Interface.Naming_Context_Ref;
      Obj : Objects.Object_Ref)
     return Bound_Object;
   --  Depending whether Ctx or Obj is null, return a bound object with
   --  appropriate discriminant.

   procedure Free is
     new Ada.Unchecked_Deallocation
     (Bound_Objects_Sequence, Bound_Objects_List);

   procedure Free is
     new Ada.Unchecked_Deallocation
     (Binding_Iterator, Binding_Iterator_Ptr);

   procedure Free_Bound_Objects_List
     (X : in out Bound_Objects_List);
   --  Intermediate procedure to trace memory leaks.

   function Look_For_BO_In_BOL
     (BOL : Bound_Objects_List;
      N   : Name;
      Ctx : Boolean;
      Obj : Boolean)
     return Natural;
   --  Return index in bound objects list where N is located. Check also BO
   --  type. Ctx and Obj indicate whether we want a context, an object or
   --  any of them. Return 0 when there is no match.

   procedure Msg
     (Ctx : Naming_Context;
      S1  : String;
      N   : Name   := (True, null);
      S2  : String := "");
   --  To debug.

   function New_Bound_Objects_Sequence
     (F, L : Natural)
     return Bound_Objects_List;
   --  To keep track of allocation.

   procedure Remove_BO_From_BOL
     (BOL : in out Bound_Objects_List;
      Idx : in Natural);
   --  Remove from bound objects list component of index Idx.

   ----------------------
   -- Append_BO_To_BOL --
   ----------------------

   procedure Append_BO_To_BOL
     (BOL : in out Bound_Objects_List;
      N   : in Name;
      Ctx : in Interface.Naming_Context_Ref;
      Obj : in Objects.Object_Ref)
   is
      Old : Bound_Objects_List;
      BO  : Bound_Object := Create (N.NCList (N.NCList'Last), Ctx, Obj);
   begin
      if BOL = null then
         BOL := New_Bound_Objects_Sequence (1, 1);

      else
         Old := BOL;
         BOL := New_Bound_Objects_Sequence (Old'First, Old'Last + 1);
         BOL (Old'Range) := Old.all;
         Free_Bound_Objects_List (Old);
      end if;

      BOL (BOL'Last) := BO;
   end Append_BO_To_BOL;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Ctx : access Naming_Context;
      N   : in Name;
      Obj : in Objects.Object_Ref)
   is
      Prefix : Name;
      Remain : Name;
      Index  : Natural;
      Id     : Exception_Id := Null_Id;
   begin
      if N.NCList = null
        or else N.NCList'Length = 0
      then
         Id := Invalid_Name'Identity;

      elsif Obj = null then
         Id := Cannot_Proceed'Identity;

      else
         Get_Prefix_From_Name (N, Prefix, Remain);
         pragma Debug (Msg (Ctx.all, "->Bind (", N, ", O)"));

         if Remain.NCList = null then
            Index := Look_For_BO_In_BOL (Ctx.BOL, Prefix, True, True);
            if Index = 0 then
               Append_BO_To_BOL (Ctx.BOL, Prefix, null, Obj);

            else
               Id := Already_Bound'Identity;
            end if;

         else
            Index := Look_For_BO_In_BOL (Ctx.BOL, Prefix, True, False);
            if Index = 0 then
               Id := Not_Found'Identity;

            else
               begin
                  Bind (Ctx.BOL (Index).Ctx, Remain, Obj);
               exception when E : others =>
                  Id := Exception_Identity (E);
               end;
            end if;
         end if;
      end if;

      Free_Non_Null_Name (Prefix);
      Free_Non_Null_Name (Remain);
      Free_Remote_Name   (N);
      Raise_Exception (Id);
   end Bind;

   ------------------
   -- Bind_Context --
   ------------------

   procedure Bind_Context
     (Ctx : access Naming_Context;
      N   : in Name;
      NC  : in Interface.Naming_Context_Ref)
   is
      Prefix : Name;
      Remain : Name;
      Index  : Natural;
      Id     : Exception_Id := Null_Id;
   begin
      if N.NCList = null
        or else N.NCList'Length = 0
      then
         Id := Invalid_Name'Identity;

      elsif NC = null then
         Id := Cannot_Proceed'Identity;

      else
         Get_Prefix_From_Name (N, Prefix, Remain);
         pragma Debug (Msg (Ctx.all, "->Bind_Context (", N, ", NC)"));

         if Remain.NCList = null then
            Index := Look_For_BO_In_BOL (Ctx.BOL, Prefix, True, True);
            if Index = 0 then
               Append_BO_To_BOL (Ctx.BOL, Prefix, NC, null);

            else
               Id := Already_Bound'Identity;
            end if;

         else
            Index := Look_For_BO_In_BOL (Ctx.BOL, Prefix, True, False);
            if Index = 0 then
               Id := Not_Found'Identity;

            else
               begin
                  Bind_Context (Ctx.BOL (Index).Ctx, Remain, NC);
               exception when E : others =>
                  Id := Exception_Identity (E);
               end;
            end if;
         end if;
      end if;

      Free_Non_Null_Name (Prefix);
      Free_Non_Null_Name (Remain);
      Free_Remote_Name   (N);
      Raise_Exception (Id);
   end Bind_Context;

   ----------------------
   -- Bind_New_Context --
   ----------------------

   procedure Bind_New_Context
     (Ctx : access Naming_Context;
      N   : in Name)
   is
      Id : Exception_Id := Null_Id;
   begin
      if N.NCList = null
        or else N.NCList'Length = 0
      then
         Id := Invalid_Name'Identity;

      else
         begin
            Bind_Context (Ctx, N, New_Context (Ctx));
         exception when E : others =>
            Id := Exception_Identity (E);
         end;
      end if;

      Free_Remote_Name (N);
      Raise_Exception (Id);
   end Bind_New_Context;

   ------------
   -- Create --
   ------------

   function Create
     (NC  : Name_Component;
      Ctx : Interface.Naming_Context_Ref;
      Obj : Objects.Object_Ref)
     return Bound_Object
   is
   begin
      if Obj = null  then
         declare
            X : Bound_Object (Naming_Context_Type);
         begin
            X.BNC := Copy (NC);
            X.Ctx := Ctx;
            return X;
         end;

      else
         declare
            X : Bound_Object (Object_Type);
         begin
            X.BNC := Copy (NC);
            X.Obj := Obj;
            return X;
         end;
      end if;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Ctx : access Naming_Context)
   is
   begin
      if Ctx.BOL /= null then
         if Ctx.BOL'Length /= 0 then
            raise Not_Empty;
         else
            Free_Bound_Objects_List (Ctx.BOL);
         end if;
      end if;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (BI : access Binding_Iterator)
   is
   begin
      null;
   end Destroy;

   -----------------------------
   -- Free_Bound_Objects_List --
   -----------------------------

   procedure Free_Bound_Objects_List
     (X : in out Bound_Objects_List)
   is
   begin
      if X /= null then
         pragma Debug (Trace ("BOS", -X'Length, Bound_Object'Size));
         Free (X);
      end if;
   end Free_Bound_Objects_List;

   ---------------
   -- Get_Image --
   ---------------

   function Get_Image
     (Ctx : access Naming_Context)
      return String is
   begin
      return Get_Istring (Ctx.Img);
   end Get_Image;

   ----------
   -- List --
   ----------

   procedure List
     (Ctx      : access Naming_Context;
      How_Many : in  Natural;
      BL       : out Binding_List;
      BI       : out Interface.Binding_Iterator_Ref)
   is
      Continue : Boolean;
      Iterator : Binding_Iterator_Ptr;

      --  Note that once a remote call to this subprogram completes,
      --  Binding_List'Write will be called and BL will be deallocated.
      --  We do not need a complex mechanism like Name's one because
      --  Binding_List parameters are always "out" parameters.

   begin
      if Ctx.BOL = null then
         BL := null;
         BI := null;

      else
         pragma Debug (Msg (Ctx.all, "->List()"));
         Iterator     := new Binding_Iterator;
         Iterator.Ctx := Ctx.all'Access;
         Iterator.Idx := 0;

         Next_N (Iterator.all'Access, How_Many, BL, Continue);

         if not Continue then
            BI := Iterator.all'Access;

         else
            Free (Iterator);
            BI := null;
         end if;
      end if;
   end List;

   ------------------------
   -- Look_For_BO_In_BOL --
   ------------------------

   function Look_For_BO_In_BOL
     (BOL : Bound_Objects_List;
      N   : Name;
      Ctx : Boolean;
      Obj : Boolean)
      return Natural
   is
      Id : constant String := N.NCList (N.NCList'Last).Id.all;
      Ok : array (Binding_Type) of Boolean;
   begin
      if BOL = null
        or else BOL'Length = 0
      then
         return 0;
      end if;

      Ok (Object_Type)         := Obj;
      Ok (Naming_Context_Type) := Ctx;

      for I in BOL'Range loop
         if BOL (I).BNC.Id.all = Id
           and then Ok (BOL (I).BT)
         then
            return I;
         end if;
      end loop;
      return 0;
   end Look_For_BO_In_BOL;

   ---------
   -- Msg --
   ---------

   procedure Msg
     (Ctx : Naming_Context;
      S1  : String;
      N   : Name   := (True, null);
      S2  : String := "")
   is
   begin
      System.IO.Put ('"'); -- "
      System.IO.Put (Get_Istring (Ctx.Img));
      System.IO.Put ('"'); -- "
      System.IO.Put (S1);
      if N.NCList /= null then
         System.IO.Put ('"'); -- "
         for I in N.NCList'First .. N.NCList'Last - 1 loop
            if N.NCList (I).Id /= null then
               System.IO.Put (N.NCList (I).Id.all);
               System.IO.Put ('/');
            end if;
         end loop;
         System.IO.Put (N.NCList (N.NCList'Last).Id.all);
         System.IO.Put ('"'); -- "
      end if;
      System.IO.Put_Line (S2);
   end Msg;

   --------------------------------
   -- New_Bound_Objects_Sequence --
   --------------------------------

   function New_Bound_Objects_Sequence
     (F, L : Natural)
     return Bound_Objects_List
   is
   begin
      pragma Debug (Trace ("BOS", L - F + 1, Binding'Size));
      return new Bound_Objects_Sequence (F .. L);
   end New_Bound_Objects_Sequence;

   -----------------
   -- New_Context --
   -----------------

   function New_Context
      return Interface.Naming_Context_Ref
   is
      New_Ctx : Naming_Context_Ptr := new Naming_Context;
   begin
      return New_Ctx.all'Access;
   end New_Context;

   -----------------
   -- New_Context --
   -----------------

   function New_Context
     (Ctx : access Naming_Context)
      return Interface.Naming_Context_Ref
   is
      New_Ctx : Naming_Context_Ptr := new Naming_Context;
   begin
      return New_Ctx.all'Access;
   end New_Context;

   ------------
   -- Next_N --
   ------------

   procedure Next_N
     (BI       : access Binding_Iterator;
      How_Many : in Natural;
      BL       : out Binding_List;
      Done     : out Boolean)
   is
      Idx : Natural renames BI.Idx;
      BOL : Bound_Objects_List renames BI.Ctx.BOL;
      Len : Natural := How_Many;

      --  Note that once a remote call to this subprogram completes,
      --  Binding_List'Write will be called and BL will be deallocated.
      --  We do not need a complex mechanism like Name's one because
      --  Binding_List parameters are always "out" parameters.

   begin
      if BOL = null then
         BL   := null;
         Done := False;

      else
         pragma Debug (Msg (BI.Ctx.all, "->Next_N()"));
         if Idx + How_Many > BOL'Last then
            Len := How_Many - Idx;
         end if;

         if Len = 0 then
            BL   := null;
            Done := False;

         else
            BL := New_Binding_Sequence (1, Len);
            for I in BL'Range loop
               Idx := Idx + 1;
               BL (I).BN := Copy ((1 .. 1 => BOL (Idx).BNC));
               BL (I).BT := BOL (Idx).BT;
            end loop;

            if BOL'Last <= Idx then
               Done := False;
            else
               Done := True;
            end if;
         end if;
      end if;
   end Next_N;

   --------------
   -- Next_One --
   --------------

   procedure Next_One
     (BI   : access Binding_Iterator;
      B    : out Binding;
      Done : out Boolean)
   is
      BL : Binding_List;
   begin
      pragma Debug (Msg (BI.Ctx.all, "->Next_One()"));
      Next_N (BI, 1, BL, Done);
      if BL /= null then
         B := BL (BL'First);
         Free_Binding_List (BL);
      end if;
   end Next_One;

   ------------
   -- Rebind --
   ------------

   procedure Rebind
     (Ctx : access Naming_Context;
      N   : in Name;
      Obj : in Objects.Object_Ref)
   is
      Id : Exception_Id := Null_Id;
   begin
      begin
         Unbind (Ctx, N);
      exception when Not_Found =>
         null;
      end;
      begin
         Bind (Ctx, N, Obj);
      exception when E : others =>
         Id := Exception_Identity (E);
      end;

      Free_Remote_Name (N);
      Raise_Exception (Id);
   end Rebind;

   --------------------
   -- Rebind_Context --
   --------------------

   procedure Rebind_Context
     (Ctx : access Naming_Context;
      N   : in Name;
      NC  : in Interface.Naming_Context_Ref)
   is
      Id : Exception_Id := Null_Id;
   begin
      begin
         Unbind (Ctx, N);
      exception when Not_Found =>
         null;
      end;
      begin
         Bind_Context (Ctx, N, NC);
      exception when E : others =>
         Id := Exception_Identity (E);
      end;

      Free_Remote_Name (N);
      Raise_Exception (Id);
   end Rebind_Context;

   ------------------------
   -- Remove_BO_From_BOL --
   ------------------------

   procedure Remove_BO_From_BOL
     (BOL : in out Bound_Objects_List;
      Idx : in Natural)
   is
      Old : Bound_Objects_List := BOL;
   begin
      if BOL'Length = 1 then
         Free_Istring (BOL (BOL'First).BNC.Id);
         Free_Istring (BOL (BOL'First).BNC.Kind);
         Free_Bound_Objects_List (BOL);

      else
         Old := BOL;
         BOL := New_Bound_Objects_Sequence (Old'First, Old'Last - 1);
         BOL.all := Old (Old'First .. Idx - 1) & Old (Idx + 1 .. Old'Last);
         Free_Istring (Old (Idx).BNC.Id);
         Free_Istring (Old (Idx).BNC.Kind);
         Free_Bound_Objects_List (Old);
      end if;
   end Remove_BO_From_BOL;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (Ctx : access Naming_Context;
      N   : in Name)
      return Objects.Object_Ref
   is
      Prefix : Name;
      Remain : Name;
      Index  : Natural;
      Id     : Exception_Id := Null_Id;
      Result : Objects.Object_Ref;
   begin
      if N.NCList = null
      or else N.NCList'Length = 0
      then
         Id := Invalid_Name'Identity;

      else
         Get_Prefix_From_Name (N, Prefix, Remain);
         pragma Debug (Msg (Ctx.all, "->Resolve (", N, ")"));

         if Remain.NCList = null then
            Index := Look_For_BO_In_BOL (Ctx.BOL, Prefix, False, True);
            if Index = 0 then
               Id := Not_Found'Identity;

            else
               Result := Ctx.BOL (Index).Obj;
            end if;

         else
            Index := Look_For_BO_In_BOL (Ctx.BOL, Prefix, True, False);
            if Index = 0 then
               Id := Not_Found'Identity;

            else
               begin
                  Result := Resolve (Ctx.BOL (Index).Ctx, Remain);
               exception when E : others =>
                  Id := Exception_Identity (E);
               end;
            end if;
         end if;
      end if;

      Free_Non_Null_Name (Prefix);
      Free_Non_Null_Name (Remain);
      Free_Remote_Name (N);
      Raise_Exception (Id);

      return Result;
   end Resolve;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (Ctx : access Naming_Context;
      N   : in Name)
      return Interface.Naming_Context_Ref
   is
      Prefix : Name;
      Remain : Name;
      Index  : Natural;
      Id     : Exception_Id := Null_Id;
      Result : Interface.Naming_Context_Ref;
   begin
      if N.NCList = null
      or else N.NCList'Length = 0
      then
         Id := Invalid_Name'Identity;

      else
         Get_Prefix_From_Name (N, Prefix, Remain);
         pragma Debug (Msg (Ctx.all, "->Resolve (", N, ")"));

         if Remain.NCList = null then
            Index := Look_For_BO_In_BOL (Ctx.BOL, Prefix, True, False);
            if Index = 0 then
               Id := Not_Found'Identity;

            else
               Result := Ctx.BOL (Index).Ctx;
            end if;

         else
            Index := Look_For_BO_In_BOL (Ctx.BOL, Prefix, True, False);
            if Index = 0 then
               Id := Not_Found'Identity;

            else
               begin
                  Result := Resolve (Ctx.BOL (Index).Ctx, Remain);
               exception when E : others =>
                  Id := Exception_Identity (E);
               end;
            end if;
         end if;
      end if;

      Free_Non_Null_Name (Prefix);
      Free_Non_Null_Name (Remain);
      Free_Remote_Name (N);
      Raise_Exception (Id);

      return Result;
   end Resolve;

   ---------------
   -- Set_Image --
   ---------------

   procedure Set_Image
     (Ctx : access Naming_Context;
      Img : in String) is
   begin
      Set_Istring (Ctx.Img, Img);
   end Set_Image;

   ------------
   -- Unbind --
   ------------

   procedure Unbind
     (Ctx : access Naming_Context;
      N   : in Name)
   is
      Prefix : Name;
      Remain : Name;
      Index  : Natural;
      Id     : Exception_Id := Null_Id;
   begin
      if N.NCList = null
        or else N.NCList'Length = 0
      then
         Id := Invalid_Name'Identity;

      else
         Get_Prefix_From_Name (N, Prefix, Remain);
         pragma Debug (Msg (Ctx.all, "->Unbind (", N, ")"));

         if Remain.NCList = null then
            Index := Look_For_BO_In_BOL (Ctx.BOL, Prefix, True, True);
            if Index = 0 then
               Id := Not_Found'Identity;

            else
               Remove_BO_From_BOL (Ctx.BOL, Index);
            end if;

         else
            Index := Look_For_BO_In_BOL (Ctx.BOL, Prefix, True, False);
            if Index = 0 then
               Id := Not_Found'Identity;

            else
               begin
                  Unbind (Ctx.BOL (Index).Ctx, Remain);
               exception when E : others =>
                  Id := Exception_Identity (E);
               end;
            end if;
         end if;
      end if;

      Free_Non_Null_Name (Prefix);
      Free_Non_Null_Name (Remain);
      Free_Remote_Name (N);
      Raise_Exception (Id);
   end Unbind;

   ----------
   -- Read --
   ----------

   procedure Read  (S : access Root_Stream_Type'Class;
                    X : out Bound_Objects_List)
   is
   begin
      X := null;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (S : access Root_Stream_Type'Class;
                    X : in Bound_Objects_List)
   is
   begin
      null;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read  (S : access Root_Stream_Type'Class;
                    X : out Naming_Context_Ptr)
   is
   begin
      X := null;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (S : access Root_Stream_Type'Class;
                    X : in Naming_Context_Ptr)
   is
   begin
      null;
   end Write;

end GLADE.Naming.Implementation;
