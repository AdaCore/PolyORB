------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                         G L A D E . N A M I N G                          --
--                                                                          --
--                                 S p e c                                  --
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

package GLADE.Naming is

   --  This package is an implementation of a service close to the Naming
   --  Service of CORBA using the Distributed Systems Annex of Ada95.

   pragma Remote_Types;

   -------------
   -- Istring --
   -------------

   type Istring is private;

   No_String : constant String;

   function Get_Istring
     (I : in Istring)
     return String;
   --  Returns content of I. When I is null, returns No_String.

   procedure Set_Istring
     (I : in out Istring;
      S : in String);
   --  Deallocate previous I and sets new content of I to S. If S is
   --  No_String, then I is juste deallocated.

   ----------
   -- Name --
   ----------

   type Name_Component is record
      Id   : Istring;
      Kind : Istring;
   end record;

   type Name_Component_Sequence is array (Natural range <>) of Name_Component;

   type Name is private;

   No_Name_Component_Sequence : constant Name_Component_Sequence;

   function Get_Name
     (N  : in Name)
     return Name_Component_Sequence;
   --  Returns content of N. When N is null, returns
   --  No_Name_Component_Sequence.

   procedure Set_Name
     (N  : in out Name;
      NS : in Name_Component_Sequence);
   --  Deallocate previous N and sets new content of N to NS. If S is
   --  No_Name_Component_Sequence, then N is juste deallocated.

   -------------
   -- Binding --
   -------------

   type Binding_Type is (Object_Type, Naming_Context_Type);

   type Binding is record
      BN : Name;
      BT : Binding_Type;
   end record;

   ------------------
   -- Binding_List --
   ------------------

   type Binding_Sequence is array (Natural range <>) of Binding;

   type Binding_List is private;

   No_Binding_Sequence : constant Binding_Sequence;

   function Get_Binding_List
     (BL : in Binding_List)
     return Binding_Sequence;
   --  Returns content of BL. When BL is null, returns No_Binding_Sequence.

   procedure Set_Binding_List
     (BL : in out Binding_List;
      BS : in Binding_Sequence);
   --  Deallocate previous BL and sets new content of BL to BS. If BS is
   --  No_Binding_Sequence, then BL is juste deallocated.

   ----------------
   -- Exceptions --
   ----------------

   type Not_Found_Reason is (Missing_Node, Not_Context, Not_Object);

   Not_Found      : exception;
   --  Indicates the name does not identify a binding.

   Cannot_Proceed : exception;
   --  Indicates that the implementation  has given up for some reason.

   Invalid_Name   : exception;
   --  Indicates the name is invalid.

   Already_Bound  : exception;
   --  Indicates an object is already bound to the specified name. Only one
   --  object can be bound to a particular name in a context.

   Not_Empty      : exception;

private

   -------------
   -- Istring --
   -------------

   type Istring is access String;

   procedure Read  (S : access Ada.Streams.Root_Stream_Type'Class;
                    X : out Istring);

   procedure Write (S : access Ada.Streams.Root_Stream_Type'Class;
                    X : in Istring);

   for Istring'Read  use Read;
   for Istring'Write use Write;

   No_String : constant String (1 .. 0) := (others => ' ');

   procedure Free_Istring
     (X : in out Istring);
   --  Intermediate procedure to trace memory leaks.

   function New_String
     (F, L : Natural)
     return Istring;
   --  To keep track of allocation.

   ----------
   -- Name --
   ----------

   type Name_Component_List is access Name_Component_Sequence;

   type Name is
      record
         Remote : Boolean := False;
         NCList : Name_Component_List;
      end record;
   --  Remote is set to true when Name is a copy of a remote name. This is
   --  useful for un/marhsllaing procedures to know when to deallocate a "in"
   --  name parameter.

   procedure Read  (S : access Ada.Streams.Root_Stream_Type'Class;
                    X : out Name);

   procedure Write (S : access Ada.Streams.Root_Stream_Type'Class;
                    X : in Name);

   for Name'Read  use Read;
   for Name'Write use Write;

   No_Name_Component_Sequence : constant Name_Component_Sequence (1 .. 0)
     := (others => (null, null));

   function Copy
     (NC : Name_Component)
      return Name_Component;
   --  Duplicate any access component.

   function Copy
     (NCS : Name_Component_Sequence)
      return Name;
   --  Duplicate any access component.

   procedure Free_Name_Component_List
     (X : in out Name_Component_List);
   --  Intermediate procedure to trace memory leaks.

   procedure Free_Non_Null_Name
     (N : in out Name);
   --  Check whether N is null before deallocation.

   procedure Free_Remote_Name
     (N : in Name);
   --  Check whether N is an pointer created by a marshalling procedure. If
   --  it is so, then N should be deallocated at the end of a remote
   --  procedure because it is a copy.

   procedure Get_Prefix_From_Name
     (Source : in  Name;
      Prefix : out Name;
      Remain : out Name);
   --  Split Source in two parts Prefix and Remain. Note that Name is
   --  already an array. So, we just have to copy the first item in Prefix
   --  and to copy the remaining right part in Remain. Each component is
   --  duplicated.

   function New_Name_Component_Sequence
     (F, L : Natural)
     return Name_Component_List;
   --  To keep track of allocation.

   ------------------
   -- Binding_List --
   ------------------

   type Binding_List is access Binding_Sequence;

   No_Binding_Sequence : constant Binding_Sequence (1 .. 0)
     := (others => ((True, null), Object_Type));

   procedure Read  (S : access Ada.Streams.Root_Stream_Type'Class;
                    X : out Binding_List);

   procedure Write (S : access Ada.Streams.Root_Stream_Type'Class;
                    X : in Binding_List);

   for Binding_List'Read  use Read;
   for Binding_List'Write use Write;

   procedure Free_Binding_List
     (X : in out Binding_List);
   --  Intermediate procedure to trace memory leaks.

   function New_Binding_Sequence
     (F, L : Natural)
     return Binding_List;
   --  To keep track of allocation.

   procedure Trace
     (Msg  : String;
      Len  : Integer;
      Size : Natural);
   --  Keep track of allocation and deallocation of arrays.

end GLADE.Naming;
