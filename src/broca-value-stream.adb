------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--               B R O C A . V A L U E . V A L U E _ S T R E A M            --
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

with CORBA.Value;
with CORBA.Impl;

with Broca.Exceptions;

with Broca.Opaque;
with Broca.Buffers;  use Broca.Buffers;

with Broca.CDR; use Broca.CDR;

with Broca.Debug;

package body Broca.Value.Stream is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.value.stream");
   procedure O is new Broca.Debug.Output (Flag);

   function CDR_Position (Buffer : access Buffer_Type)
                          return CORBA.Long;
   --  return the current CDR position of the buffer
   --  in the marshalling stream as a CORBA.Long. Raises
   --  CORBA::Marshall if it is too big to fit in a CORBA.Long.


   --------------------------------------
   --  end of subprogram declarations  --
   --------------------------------------


   -------------
   --  Match  --
   -------------
   function Match
     (Item : in Indirection_Element;
      Needle : in CORBA.AbstractBase.Ref)
      return Standard.Boolean is
      use CORBA.Impl;
   begin
      return
        (Item.Type_Id = Value_Ref
         and then
         CORBA.AbstractBase.Object_Of (Item.Ref)
         = CORBA.AbstractBase.Object_Of (Needle));
   end Match;


   -------------
   --  Match  --
   -------------
   function Match (Item : in Indirection_Element;
                   Needle : in CORBA.Long) return Standard.Boolean is
      use CORBA;
   begin
      return Item.Offset = Needle;
   end Match;

   --------------------
   --  CDR_Position  --
   --------------------
   function CDR_Position (Buffer : access Buffer_Type)
                          return CORBA.Long is
      Index : constant Broca.Opaque.Index_Type
        := Broca.Buffers.CDR_Position (Buffer);
      use Broca.Opaque;
   begin
      if Index >= 2**31 then
         Broca.Exceptions.Raise_Marshal;
      else
         return CORBA.Long (Index);
      end if;
   end CDR_Position;

   ---------------------------
   --  Marshall_Indirection --
   ---------------------------
   procedure Marshall_Indirection
     (Buffer : access Buffer_Type;
      Already_Marshalled : in out ISeq.Sequence;
      Val : in CORBA.Value.Base'Class;
      Success : out CORBA.Boolean) is
      Index : constant Natural
        := ISS_Ptr.Index (Already_Marshalled,
                          CORBA.AbstractBase.Ref (Val));
      Element : Indirection_Element;
   begin
      pragma Debug (O ("Marshall_Indirection: enter, Index = "
                       & Natural'Image (Index)));
      if Index = 0 then
         --  This is the first time this object is marshalled
         --  align the buffer to marshall a CORBA.Long on 4 octets
         Broca.Buffers.Align (Buffer,
                              Broca.Opaque.Alignment_Type' (4));
         pragma Debug (O ("Marshall_Indirection: CDR_Position = "
                          & CORBA.Long'Image (CDR_Position (Buffer))));
         Element.Offset := CDR_Position (Buffer);
         Element.Type_Id := Value_Ref;
         Element.Ref := CORBA.AbstractBase.Ref (Val);
         ISeq.Append (Already_Marshalled, Element);
         Success := CORBA.Boolean (False);
         pragma Debug (O ("Marshall_Indirection: Already_Marshalled'Length = "
                          & Natural'Image (ISeq.Length (Already_Marshalled))));
      else
         --  This is *not* the first time this object is marshalled
         Marshall (Buffer, Indirection_Tag);
         Element := ISeq.Element_Of (Already_Marshalled, Index);
         pragma Debug (O ("Marshall_Indirection: Element.Offset = "
                          & CORBA.Long'Image (Element.Offset)));
         pragma Debug (O ("Marshall_Indirection: CDR_Position = "
                          & CORBA.Long'Image (CDR_Position (Buffer))));
         pragma Debug (O ("Marshall_Indirection: ind marshalled = "
                          & CORBA.Long'Image
                          (Element.Offset - CDR_Position (Buffer))));
         Marshall (Buffer,
                   Element.Offset - CDR_Position (Buffer));
         Success := CORBA.Boolean (True);
      end if;
   end Marshall_Indirection;


   ----------------
   --  Finalize  --
   ----------------
   procedure Finalize (El : in out Indirection_Element) is
   begin
      case El.Type_Id is
         when Codebase_Url =>
            null;

         when Value_Ref =>
            null;

         when Repository_Id =>
            Free (El.RepoId);

         when Repository_Id_List =>
            Free (El.RepoId_List);
      end case;
   end Finalize;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Value.Base'Class) is
      Already_Marshalled : ISeq.Sequence
        := ISeq.Null_Sequence;
   begin
      --  FIXME: the formal repoid is the actual type
      --  of Data. Get Data's repositoryId
      Marshall
        (Buffer,
         Data,
         CORBA.Null_RepositoryId,
         Already_Marshalled,
         CORBA.Long (1));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Value.Base'Class;
      Formal : in CORBA.RepositoryId;
      Already_Marshalled : in out ISeq.Sequence;
      Nesting_Depth : in CORBA.Long) is
   begin
      if CORBA.Value.Is_Nil (Data) then
         Marshall (Buffer, Null_Tag);
      else
         declare
            Success : Boolean;
         begin
            Marshall_Indirection
              (Buffer,
               Already_Marshalled,
               Data,
               Success);
            if not Success then
               declare
                  Operation : Marshall_Type;
               begin
                  Operation := Marshall_Store.Get_Operation
                    (CORBA.Value.Object_Of (Data).all'Tag);
                  Operation (Buffer,
                             CORBA.Value.Object_Of (Data),
                             Already_Marshalled,
                             Formal,
                             False,
                             Nesting_Depth);
               end;
            end if;
         end;
      end if;
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Formal : in CORBA.RepositoryId;
      Result : in out CORBA.Value.Base'Class) is
      Already_Unmarshalled : ISeq.Sequence
        := ISeq.Null_Sequence;
      Closing_Tag_Read : CORBA.Long;
   begin
      Unmarshall
        (Buffer,
         Formal,
         Result,
         Already_Unmarshalled,
         False,
         CORBA.Long (1),
         Closing_Tag_Read);
   end Unmarshall;

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Formal : in CORBA.RepositoryId;
      Result : in out CORBA.Value.Base'Class;
      Already_Unmarshalled : in out ISeq.Sequence;
      With_Chunking : in Boolean;
      Nesting_Depth : in CORBA.Long;
      Closing_Tag_Read : out CORBA.Long) is
      Value_Tag : constant CORBA.Long
        := Unmarshall (Buffer);
      use CORBA;
      Null_Ptr : CORBA.Impl.Object_Ptr := null;
   begin
      pragma Debug (O ("Unmarshall, Value_Tag = "
                       & CORBA.Long'Image (Value_Tag)));
      --  null tag
      if Value_Tag = Null_Tag then
         CORBA.Value.Set (CORBA.Value.Base (Result), Null_Ptr);

         --  indirection
      elsif Value_Tag = Indirection_Tag then
         declare
            Ind_Offset : CORBA.Long;
            First_Occ : CORBA.Long;
            Current_Pos : CORBA.Long;
            Index : Natural;
         begin
            Current_Pos := CDR_Position (Buffer);
            Ind_Offset := Unmarshall (Buffer);
            pragma Debug (O ("Unmarshall indirection, ind_offset read = "
                             & CORBA.Long'Image (Ind_Offset)));
            pragma Debug (O ("Unmarshall indirection, CDR_Position = "
                             & CORBA.Long'Image (Current_Pos)));
            First_Occ :=  Current_Pos + Ind_Offset;
            pragma Debug (O ("Unmarshall indirection, first occurence is at "
                             & CORBA.Long'Image (First_Occ)));
            Index := ISS_Offset.Index (Already_Unmarshalled,
                                       First_Occ);
            pragma Debug (O ("Unmarshall indirection, index of first occ = "
                             & Natural'Image (Index)));
            if Index = 0 then
               Broca.Exceptions.Raise_Marshal;
            else
               declare
                  El : Indirection_Element;
                  R : CORBA.AbstractBase.Ref;
                  P : CORBA.Impl.Object_Ptr;
               begin
                  El := ISeq.Element_Of (Already_Unmarshalled, Index);
                  R := El.Ref;
                  P := CORBA.AbstractBase.Object_Of (R);
                  CORBA.AbstractBase.Set
                    (CORBA.AbstractBase.Ref (Result), P);
               end;
               --  To keep compiler happy
               --  (Closing_Tag_Read not implemented yet)
               Closing_Tag_Read := 0;
            end if;
         end;

         --  standard marshalling
      else

         --  if there is a codebase URL, skip it
         --  we suppose there is no type information
         declare
            Operation : Unmarshall_Fields_Type
              := Unmarshall_Fields_Store.Get_Operation (Formal);
         begin
            --  initialize the value in case it was not done
            CORBA.Value.Set (CORBA.Value.Base (Result), Null_Ptr);
            Operation (Buffer,
                       Result,
                       Already_Unmarshalled,
                       With_Chunking,
                       Nesting_Depth,
                       Closing_Tag_Read);
         end;
      end if;
   end Unmarshall;

   --------------
   --  Append  --
   --------------
   procedure Append
     (Buffer : access Buffer_Type;
      Already_Unmarshalled : in out ISeq.Sequence;
      Data : in CORBA.Value.Base'Class) is
      El : Indirection_Element;
   begin
      --  Add the freshly unmarshalled object
      --  to the Already_Unmarshalled sequence
      El.Offset := CDR_Position (Buffer) - 4;
      --  (-4) because the tag has already been unmarshalled
      El.Type_Id := Value_Ref;
      El.Ref := CORBA.AbstractBase.Ref (Data);
      ISeq.Append (Already_Unmarshalled, El);
      pragma Debug (O ("Append: VT unmarshalled at offset = "
                       & CORBA.Long'Image (El.Offset)));
      pragma Debug (O ("Append: Already_Unmarshalled'Length = "
                       & Natural'Image
                       (ISeq.Length (Already_Unmarshalled))));
   end Append;

end Broca.Value.Stream;





