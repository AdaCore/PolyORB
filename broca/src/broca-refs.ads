------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           B R O C A . R E F S                            --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Finalization;
with Broca.Buffers;

package Broca.Refs is

   pragma Elaborate_Body;

   type Entity is abstract tagged limited private;
   --  Entity is the base type of all objects that can be
   --  referenced. It contains a Counter, which is the number of
   --  references to this object, and is automatically destroyed when
   --  the counter reaches 0.

   procedure Marshall
     (Buffer : access Broca.Buffers.Buffer_Type;
      Value  : in Entity);

   procedure Unmarshall
     (Buffer : access Broca.Buffers.Buffer_Type;
      Value  : out Entity);

   type Ref_Ptr is access all Entity'Class;

   type Ref is new Ada.Finalization.Controlled with private;
   --  The base type of all references. Inside CORBA (and Broca), this
   --  type is often derived but never extended. It contains one
   --  field, which designate the referenced object.

   function Get (Self : Ref) return Ref_Ptr;
   --  Get inner Entity object

   procedure Set (Self : in out Ref; Referenced : Ref_Ptr);
   --  Set the object (can destroyed the previous one, if it was the only
   --  reference).

   --  Handle the usage counter, unless Obj is null or the counter is
   --  disabled.
   procedure Inc_Usage (Obj : in Ref_Ptr);
   procedure Dec_Usage (Obj : in out Ref_Ptr);

private

   type Entity is abstract tagged limited
      record
         Counter : Integer := 0;
      end record;

   type Ref is new Ada.Finalization.Controlled with
      record
         A_Ref : Ref_Ptr := null;
      end record;

   procedure Initialize (Object : in out Ref);
   procedure Adjust (Object : in out Ref);
   procedure Finalize (Object : in out Ref);

end Broca.Refs;
