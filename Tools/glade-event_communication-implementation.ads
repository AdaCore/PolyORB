------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
-- G L A D E . E V E N T _ C O M M U N I C A T I O N . I M P L E M E N T A T I O N  --
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
with GLADE.Event_Communication.Interface;

package GLADE.Event_Communication.Implementation is

   pragma Remote_Types;


   -------------------
   -- Push_Consumer --
   -------------------

   type Push_Consumer is new Interface.Push_Consumer with private;

   function Create
     return Interface.Push_Consumer_Ref;

   procedure Disconnect
     (Consumer : access Push_Consumer);

   function Get
     (Consumer : access Push_Consumer)
     return Ada.Streams.Stream_Element_Array;

   procedure Push
     (Consumer : access Push_Consumer;
      Event    : in Ada.Streams.Stream_Element_Array);


   -------------------
   -- Push_Supplier --
   -------------------

   type Push_Supplier is new Interface.Push_Supplier with private;

   function Create
     return Interface.Push_Supplier_Ref;

   procedure Disconnect
     (Supplier : access Push_Supplier);

   procedure Put
     (Supplier : access Push_Supplier;
      Event    : in Ada.Streams.Stream_Element_Array);


   -------------------
   -- Pull_Consumer --
   -------------------

   type Pull_Consumer is new Interface.Pull_Consumer with private;

   function Create
     return Interface.Pull_Consumer_Ref;

   procedure Disconnect
     (Consumer : access Pull_Consumer);

   function Get
     (Consumer : access Pull_Consumer)
     return Ada.Streams.Stream_Element_Array;


   -------------------
   -- Pull_Supplier --
   -------------------

   type Pull_Supplier is new Interface.Pull_Supplier with private;

   function Create
     return Interface.Pull_Supplier_Ref;

   procedure Disconnect
     (Supplier : access Pull_Supplier);

   function Pull
     (Supplier : access Pull_Supplier)
      return Ada.Streams.Stream_Element_Array;

   procedure Put
     (Supplier : access Pull_Supplier;
      Event    : Ada.Streams.Stream_Element_Array);

   function Try_Pull
     (Supplier : access Pull_Supplier)
      return Ada.Streams.Stream_Element_Array;

private

   -------------------
   -- Push_Consumer --
   -------------------

   type Push_Consumer_Record;
   type Push_Consumer_Access is access Push_Consumer_Record;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Push_Consumer_Access);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Push_Consumer_Access);

   for Push_Consumer_Access'Read  use Read;
   for Push_Consumer_Access'Write use Write;

   type Push_Consumer is new Interface.Push_Consumer with
     record
        X : Push_Consumer_Access;
     end record;

   -------------------
   -- Pull_Consumer --
   -------------------

   type Pull_Consumer_Record;
   type Pull_Consumer_Access is access Pull_Consumer_Record;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Pull_Consumer_Access);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Pull_Consumer_Access);

   for Pull_Consumer_Access'Read  use Read;
   for Pull_Consumer_Access'Write use Write;

   type Pull_Consumer is new Interface.Pull_Consumer with
     record
        X : Pull_Consumer_Access;
     end record;

   -------------------
   -- Push_Supplier --
   -------------------

   type Push_Supplier_Record;
   type Push_Supplier_Access is access Push_Supplier_Record;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Push_Supplier_Access);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Push_Supplier_Access);

   for Push_Supplier_Access'Read  use Read;
   for Push_Supplier_Access'Write use Write;

   type Push_Supplier is new Interface.Push_Supplier with
     record
        X : Push_Supplier_Access;
     end record;

   -------------------
   -- Pull_Supplier --
   -------------------

   type Pull_Supplier_Record;
   type Pull_Supplier_Access is access Pull_Supplier_Record;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Pull_Supplier_Access);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Pull_Supplier_Access);

   for Pull_Supplier_Access'Read  use Read;
   for Pull_Supplier_Access'Write use Write;

   type Pull_Supplier is new Interface.Pull_Supplier with
     record
        X : Pull_Supplier_Access;
     end record;

end GLADE.Event_Communication.Implementation;
