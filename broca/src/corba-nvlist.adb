------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        C O R B A . N V L I S T                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Broca.CDR;
with Broca.Debug;

package body CORBA.NVList is

   Flag : constant Natural
     := Broca.Debug.Is_Active ("corba.nvlist");
   procedure O is new Broca.Debug.Output (Flag);

   ----------------
   --  Add_Item  --
   ----------------
   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item_Type  : in CORBA.TypeCode.Object;
      Value      : in System.Address;
      Len        : in Long;
      Item_Flags : in Flags) is
   begin
      null;
   end Add_Item;

   ----------------
   --  Add_Item  --
   ----------------
   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item       : in CORBA.Any;
      Item_Flags : in Flags) is
   begin
      pragma Debug (O ("Add_Item (4 params) : enter"));
      Add_Item (Self, (Name => Item_Name,
                       Argument => Item,
                       Arg_Modes => Item_Flags));
      pragma Debug (O ("Add_Item (4 params) : end"));
   end Add_Item;

   ----------------
   --  Add_Item  --
   ----------------
   procedure Add_Item
     (Self : Ref;
      Item : NamedValue) is
      List : NV_List := Get_NVList (Self);
   begin
      pragma Debug (O ("Add_Item (2 params) : enter"));
      Add_Cell (List, Item);
      if Length (List) = 1 then
         Add_NVList (Self, List);
      end if;
      pragma Debug (O ("Add_Item (2 params) : end"));
   end Add_Item;

   ------------
   --  Free  --
   ------------
   procedure Free (Self : Ref)
     renames Remove_NVList;

   -------------------
   --  Free_Memory  --
   -------------------
   procedure Free_Memory (Self : Ref)
     renames Remove_NVList;

   -----------------
   --  Get_Count  --
   -----------------
   function Get_Count (Self : Ref) return CORBA.Long is
   begin
      return Length (Get_NVList (Self));
   end Get_Count;

   ----------------
   --  Marshall  --
   ----------------
   procedure Marshall
     (Buffer : access Broca.Buffers.Buffer_Type;
      Data   : Ref) is
      List : NV_List := Get_NVList (Data);
   begin
      pragma Debug (O ("Marshall : enter"));
      while List /= Null_NVList loop
         pragma Debug (O ("Marshall : dealing with a named value"));
         if List.NV.Arg_Modes = CORBA.ARG_IN or
           List.NV.Arg_Modes = CORBA.ARG_INOUT then
            pragma Debug (O ("Marshall : marshalling a named value"));
            Broca.CDR.Marshall (Buffer, List.NV);
         end if;
         List := List.Next;
      end loop;
      pragma Debug (O ("Marshall : end"));
   end Marshall;

   ------------------
   --  Unmarshall  --
   ------------------
   procedure Unmarshall
     (Buffer : access Broca.Buffers.Buffer_Type;
      Data : in out Ref) is
      List : NV_List := Get_NVList (Data);
   begin
      pragma Debug (O ("Unmarshall : enter"));
      while List /= Null_NVList loop
         if List.NV.Arg_Modes = CORBA.ARG_OUT or
           List.NV.Arg_Modes = CORBA.ARG_INOUT then
            --  FIXME : memory leak when INOUT ?
            List.NV := Broca.CDR.Unmarshall
              (Buffer,
               List.NV.Name,
               CORBA.Get_Type (List.NV.Argument),
               List.NV.Arg_Modes);
         end if;
         List := List.Next;
      end loop;
      pragma Debug (O ("Unmarshall : end"));
   end Unmarshall;

   ------------------------------------------
   --  implementation of the private part  --
   ------------------------------------------

   ----------------------------
   --  the list of NV_Lists  --
   ----------------------------

   NVList_Repository : NVList_List := null;
   Last_NVList : NVList_List := null;

   ----------------
   --  Add_Cell  --
   ----------------
   procedure Add_Cell (List : in out NV_List;
                       NV : in NamedValue) is
      Last_Cell : NV_List := List;
   begin
      pragma Debug (O ("Add_Cell : enter"));
      pragma Debug (O ("Add_Cell : length of the list is : " &
                       Long'Image (Length (List))));
      if Last_Cell = Null_NVList then
         List := new NV_Cell;
         List.NV := NV;
         List.Next := Null_NVList;
      else
         while Last_Cell.Next /= Null_NVList loop
            Last_Cell := Last_Cell.Next;
         end loop;
         Last_Cell.Next := new NV_Cell;
         Last_Cell.Next.NV := NV;
         Last_Cell.Next.Next := Null_NVList;
      end if;
      pragma Debug (O ("Add_Cell : length of the list is : " &
                       Long'Image (Length (List))));
      pragma Debug (O ("Add_Cell : end"));
   end Add_Cell;

   ---------------------
   --  Free_All_List  --
   ---------------------
   procedure Free_All_List (List : in out NV_List) is
      Old_List : NV_List;
   begin
      while List /= Null_NVList loop
         Old_List := List;
         List := List.Next;
         Free (Old_List);
      end loop;
   end Free_All_List;

   --------------
   --  Length  --
   --------------
   function Length (List : in NV_List) return CORBA.Long is
      Result : CORBA.Long := 0;
      Current_Cell : NV_List := List;
   begin
      while Current_Cell /= Null_NVList loop
         Result := Result + 1;
         Current_Cell := Current_Cell.Next;
      end loop;
      return Result;
   end Length;

   ------------------
   --  Add_NVList  --
   ------------------
   procedure Add_NVList (Obj : Ref;
                         List : in NV_List) is
   begin
      pragma Debug (O ("Add_NVList : enter"));
      if NVList_Repository = Null_NVList_List then
         Last_NVList := new NVList_Cell;
         Last_NVList.Obj := Obj;
         Last_NVList.List := List;
         Last_NVList.Next := Null_NVList_List;
         NVList_Repository := Last_NVList;
      else
         Last_NVList.Next := new NVList_Cell;
         Last_NVList.Next.Obj := Obj;
         Last_NVList.Next.List := List;
         Last_NVList.Next.Next := Null_NVList_List;
         Last_NVList := Last_NVList.Next;
      end if;
      pragma Debug (O ("Add_NVList : end"));
   end Add_NVList;

   ---------------------
   --  Remove_NVList  --
   ---------------------
   procedure Remove_NVList (Obj : Ref) is
      Current_NVList_List : NVList_List := NVList_Repository;
      Old_NVList_List : NVList_List := Null_NVList_List;
   begin
      --  if the list is empty
      if NVList_Repository = Null_NVList_List then
         return;
      end if;
      --  goes to the right cell
      while Current_NVList_List.Obj /= Obj and
        Current_NVList_List.Next /= Null_NVList_List loop
         Old_NVList_List.Next := Current_NVList_List;
         Current_NVList_List.Next := Current_NVList_List;
      end loop;
      --  if there is a right cell
      if Current_NVList_List.Obj = Obj then
         --  frees the NVList
         Free_All_List (Current_NVList_List.List);
         if Old_NVList_List = Null_NVList_List then
            --  if the right cell is the first one, sets the repository
            NVList_Repository := Current_NVList_List.Next;
            if Current_NVList_List.Next = Null_NVList_List then
               --  if the right cell was the last one, sets last_NVList
               Last_NVList := Null_NVList_List;
            end if;
         else
            --  if the right cell was not the first one, sets the next
            --  field of the previous cell
            Old_NVList_List.Next := Current_NVList_List.Next;
            if Current_NVList_List.Next = Null_NVList_List then
               --  if the right cell was the last one, sets last_NVList
               Last_NVList := Old_NVList_List;
            end if;
         end if;
         --  frees the NVList_Cell
         Free (Current_NVList_List);
      end if;
   end Remove_NVList;

   -------------------
   --  Get_NV_List  --
   -------------------
   function Get_NVList (Obj : Ref) return NV_List is
      Current_NVList_List : NVList_List := NVList_Repository;
   begin
      while Current_NVList_List /= Null_NVList_List and then
        Current_NVList_List.Obj /= Obj loop
         Current_NVList_List := Current_NVList_List.Next;
      end loop;
      if Current_NVList_List = Null_NVList_List then
         return Null_NVList;
      else
         return Current_NVList_List.List;
      end if;
   end Get_NVList;

end CORBA.NVList;
