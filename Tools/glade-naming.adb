------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                         G L A D E . N A M I N G                          --
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
with Ada.Streams;
with System.IO;

package body GLADE.Naming is

   Leaks : Natural := 0;

   procedure Free is
     new Ada.Unchecked_Deallocation
     (String, Istring);

   procedure Free is
     new Ada.Unchecked_Deallocation
     (Name_Component_Sequence, Name_Component_List);

   procedure Free is
     new Ada.Unchecked_Deallocation
     (Binding_Sequence, Binding_List);

   ----------
   -- Copy --
   ----------

   function Copy
     (NCS : Name_Component_Sequence)
      return Name
   is
      Result : Name :=
        (False, New_Name_Component_Sequence (NCS'First, NCS'Last));
   begin
      for I in NCS'Range loop
         Result.NCList (I) := Copy (NCS (I));
      end loop;
      return Result;
   end Copy;

   ----------
   -- Copy --
   ----------

   function Copy
     (NC : Name_Component)
      return Name_Component
   is
      X : Name_Component;
   begin
      if NC.Id /= null then
         Set_Istring (X.Id, NC.Id.all);
      end if;
      if NC.Kind /= null then
         Set_Istring (X.Kind, NC.Kind.all);
      end if;
      return X;
   end Copy;

   -----------------------
   -- Free_Binding_List --
   -----------------------

   procedure Free_Binding_List
     (X : in out Binding_List)
   is
   begin
      if X /= null then
         pragma Debug (Trace ("BS ", -X'Length, Binding'Size));
         Free (X);
      end if;
   end Free_Binding_List;

   ------------------
   -- Free_Istring --
   ------------------

   procedure Free_Istring
     (X : in out Istring)
   is
   begin
      if X /= null then
         pragma Debug (Trace ("S  ", -X'Length, Character'Size));
         Free (X);
      end if;
   end Free_Istring;

   ------------------------------
   -- Free_Name_Component_List --
   ------------------------------

   procedure Free_Name_Component_List
     (X : in out Name_Component_List)
   is
   begin
      if X /= null then
         pragma Debug (Trace ("NCS", -X'Length, Name_Component'Size));
         Free (X);
      end if;
   end Free_Name_Component_List;

   ------------------------
   -- Free_Non_Null_Name --
   ------------------------

   procedure Free_Non_Null_Name
     (N : in out Name)
   is
   begin
      if N.NCList /= null then
         for I in N.NCList'Range loop
            if N.NCList (I).Id /= null then
               Free_Istring (N.NCList (I).Id);
            end if;
            if N.NCList (I).Kind /= null then
               Free_Istring (N.NCList (I).Kind);
            end if;
         end loop;
         Free_Name_Component_List (N.NCList);
      end if;
   end Free_Non_Null_Name;

   ----------------------
   -- Free_Remote_Name --
   ----------------------

   procedure Free_Remote_Name
     (N : in Name)
   is
      X : Name := N;
   begin
      if X.Remote then
         Free_Non_Null_Name (X);
      end if;
   end Free_Remote_Name;

   ----------------------
   -- Get_Binding_List --
   ----------------------

   function Get_Binding_List
     (BL : in Binding_List)
     return Binding_Sequence
   is
   begin
      if BL = null then
         return No_Binding_Sequence;
      end if;
      return BL.all;
   end Get_Binding_List;

   -----------------
   -- Get_Istring --
   -----------------

   function Get_Istring
     (I : in Istring)
      return String
   is
   begin
      if I = null then
         return "";
      end if;
      return I.all;
   end Get_Istring;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (N : in Name)
      return Name_Component_Sequence
   is
   begin
      if N.NCList = null then
         return No_Name_Component_Sequence;
      end if;
      return N.NCList.all;
   end Get_Name;

   --------------------------
   -- Get_Prefix_From_Name --
   --------------------------

   procedure Get_Prefix_From_Name
     (Source : in  Name;
      Prefix : out Name;
      Remain : out Name)
   is
   begin
      Prefix :=
        Copy (Source.NCList (Source.NCList'First .. Source.NCList'First));
      if Source.NCList'Length = 1 then
         Remain := (False, null);

      else
         Remain := Copy (Source.NCList (Source.NCList'First + 1 ..
                                      Source.NCList'Last));
      end if;
   end Get_Prefix_From_Name;

   --------------------------
   -- New_Binding_Sequence --
   --------------------------

   function New_Binding_Sequence
     (F, L : Natural)
      return Binding_List
   is
   begin
      pragma Debug (Trace ("BS ", L - F + 1, Binding'Size));
      return new Binding_Sequence (F .. L);
   end New_Binding_Sequence;

   ---------------------------------
   -- New_Name_Component_Sequence --
   ---------------------------------

   function New_Name_Component_Sequence
     (F, L : Natural)
     return Name_Component_List
   is
   begin
      pragma Debug (Trace ("NCS", L - F + 1, Name_Component'Size));
      return new Name_Component_Sequence (F .. L);
   end New_Name_Component_Sequence;

   ----------------
   -- New_String --
   ----------------

   function New_String
     (F, L : Natural)
     return Istring
   is
   begin
      pragma Debug (Trace ("S  ", L - F + 1, Character'Size));
      return new String (F .. L);
   end New_String;

   ----------------------
   -- Set_Binding_List --
   ----------------------

   procedure Set_Binding_List
     (BL : in out Binding_List;
      BS : in Binding_Sequence)
   is
   begin
      if BL /= null then
         Free_Binding_List (BL);
      end if;
      if BS /= No_Binding_Sequence then
         BL := New_Binding_Sequence (BS'First, BS'Last);
         for I in BS'Range loop
            BL (I).BN        := Copy (BS (I).BN.NCList.all);
            BL (I).BN.Remote := False;
            BL (I).BT        := BS (I).BT;
         end loop;
      end if;
   end Set_Binding_List;

   -----------------
   -- Set_Istring --
   -----------------

   procedure Set_Istring
     (I : in out Istring;
      S : in String)
   is
   begin
      if I /= null then
         Free_Istring (I);
      end if;
      if S /= No_String then
         I := New_String (S'First, S'Last);
         I.all := S;
      end if;
   end Set_Istring;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (N  : in out Name;
      NS : in Name_Component_Sequence)
   is
   begin
      Free_Non_Null_Name (N);
      if NS'Length /= 0 then
         N        := Copy (NS);
         N.Remote := False;
      end if;
   end Set_Name;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Msg  : String;
      Len  : Integer;
      Size : Natural)
   is
   begin
      Leaks := Leaks + Len * Size / 8;
      System.IO.Put_Line (Msg & " " & Len'Img & ", Total:" & Leaks'Img);
   end Trace;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Istring)
   is
      Empty : Boolean;
   begin
      Boolean'Read (S, Empty);
      if not Empty then
         declare
            I : String := String'Input (S);
         begin
            X := New_String (I'First, I'Last);
            X.all := I;
         end;
      else
         X := null;
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Istring)
   is
   begin
      if X /= null then
         Boolean'Write (S, False);
         String'Output (S, X.all);
      else
         Boolean'Write (S, True);
      end if;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Name)
   is
      Empty  : Boolean;
   begin
      X.Remote := True;
      Boolean'Read (S, Empty);
      if not Empty then
         declare
            I : Name_Component_Sequence := Name_Component_Sequence'Input (S);
         begin
            X.NCList := New_Name_Component_Sequence (I'First, I'Last);
            X.NCList.all := I;
         end;
      else
         X.NCList := null;
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Name)
   is
      N : Name := X;
   begin
      if N.NCList /= null then
         Boolean'Write (S, False);
         Name_Component_Sequence'Output (S, N.NCList.all);
         Free_Remote_Name (N);
      else
         Boolean'Write (S, True);
      end if;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Binding_List)
   is
      Empty : Boolean;
   begin
      Boolean'Read (S, Empty);
      if not Empty then
         declare
            I : Binding_Sequence := Binding_Sequence'Input (S);
         begin
            X := New_Binding_Sequence (I'First, I'Last);
            X.all := I;
         end;
      else
         X := null;
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Binding_List)
   is
      BL : Binding_List;
   begin
      if X /= null then
         Boolean'Write (S, False);
         Binding_Sequence'Output (S, X.all);
         BL := X;
         for I in BL'Range loop
            Free_Non_Null_Name (BL (I).BN);
         end loop;
         Free_Binding_List (BL);
      else
         Boolean'Write (S, True);
      end if;
   end Write;

end GLADE.Naming;
