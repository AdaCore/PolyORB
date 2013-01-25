------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    M O M A . D E S T I N A T I O N S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

--  A Destination contains data to reference an object to which messages
--  can be sent, or from which messages can be retrieved.

with MOMA.Types;

with PolyORB.Any;

package MOMA.Destinations is

   type Destination is private;
   --  Name : Logical name of the destination. When Kind is set to Topic, Name
   --         must be set to the Topic_Id.
   --  Ref  : Reference to the actual destination object.
   --  Kind : The kind of object it is really (message pool, router, ...).

   function Create_Destination
     (Name    : MOMA.Types.String;
      Ref     : MOMA.Types.Ref;
      Kind    : MOMA.Types.Destination_Type := MOMA.Types.Unknown)
      return Destination;
   --  Create a destination structure.

   overriding function "="
     (Dest1 : Destination;
      Dest2 : Destination)
     return Boolean;
   --  Compare two destinations.
   --  XXX Comparison is made only on the name.

   function Create_Destination return Destination;
   --  Create an empty destination structure.

   function Create_Temporary return Destination;
   --  Create a temporary destination
   --  XXX Not implemented.

   function Image (Self : Destination) return String;
   --  Image function for destination type.

   --  Accessors to Destination internal data.

   function Get_Name
     (Self : Destination)
      return MOMA.Types.String;

   procedure Set_Name
     (Self : in out Destination;
      Name :        MOMA.Types.String);

   function Get_Kind
     (Self : Destination)
     return MOMA.Types.Destination_Type;

   function Get_Ref
     (Self : Destination)
     return MOMA.Types.Ref;
   --  XXX should be restricted to internal use only ...

   procedure Set_Ref
     (Self : in out Destination;
      Ref  :        MOMA.Types.Ref);
   --  XXX should be restricted to internal use only ...

   --  Marshalling support for Destination type.

   TC_MOMA_Destination : PolyORB.Any.TypeCode.Local_Ref;

   function To_Any (Self : Destination) return MOMA.Types.Any;
   function From_Any (Self : MOMA.Types.Any) return Destination;

   procedure Delete;
   --  XXX really useful in this context ?

private

   type Destination is record
      Name : MOMA.Types.String;
      Ref  : MOMA.Types.Ref;
      Kind : MOMA.Types.Destination_Type;
   end record;

   pragma Inline (Get_Name);
   pragma Inline (Set_Name);
   pragma Inline (Get_Ref);
   pragma Inline (Set_Ref);
   pragma Inline (Get_Kind);

end MOMA.Destinations;
