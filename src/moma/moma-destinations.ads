------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    M O M A . D E S T I N A T I O N S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  A Destination contains data to reference an object to which messages
--  can be sent.

--  $Id$

--  XXX is a derivation for queues and topics required ?

with MOMA.Types;

with PolyORB.Any;
with PolyORB.References;

package MOMA.Destinations is

   type Destination is private;
   --  Name : Logical name of the destination
   --  Ref  : Reference to the actual destination object.

   function Create (Name : MOMA.Types.String;
                    Ref  : PolyORB.References.Ref)
                    return Destination;

   function Create return Destination;
   --  Create a destination structure.

   function Image (Self : Destination) return String;
   --  Image function for destination type.

   --  Accessors to Destination internal data.

   function Get_Name (Self : Destination)
                      return MOMA.Types.String;

   procedure Set_Name (Self : in out Destination;
                       Name : MOMA.Types.String);

   function Get_Ref (Self : Destination)
                     return PolyORB.References.Ref;
   --  XXX should be restricted to internal use only ...

   procedure Set_Ref (Self : in out Destination;
                      Ref  : PolyORB.References.Ref);
   --  XXX should be restricted to internal use only ...

   --  Marshalling support for Destination type.

   TC_MOMA_Destination : PolyORB.Any.TypeCode.Object
         := PolyORB.Any.TypeCode.TC_Struct;

   function To_Any (Self : Destination)
                    return PolyORB.Any.Any;

   function From_Any (Self : PolyORB.Any.Any)
                     return Destination;

   --  XXX check the conformance and pertinence of the above spec.
   
   procedure Delete;
   --  XXX really useful in this context ?

   type Queue is private;
   --  XXX necessary ??

private

   type Destination is record
      Name : MOMA.Types.String;
      --  Logical name of the destination

      Ref  : PolyORB.References.Ref;
      --  Reference to the actual destination object.
   end record;

   type Queue is new Destination;

   pragma Inline (Get_Name);
   pragma Inline (Set_Name);
   pragma Inline (Get_Ref);
   pragma Inline (Set_Ref);

end MOMA.Destinations;
