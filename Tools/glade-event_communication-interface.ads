------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--  G L A D E . E V E N T _ C O M M U N I C A T I O N . I N T E R F A C E   --
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

package GLADE.Event_Communication.Interface is

   pragma Remote_Types;


   -------------------
   -- Push_Consumer --
   -------------------

   type Push_Consumer is abstract tagged limited private;
   type Push_Consumer_Ref is access all Push_Consumer'Class;

   procedure Disconnect
     (Consumer : access Push_Consumer)
      is abstract;

   procedure Push
     (Consumer : access Push_Consumer;
      Event    : in Ada.Streams.Stream_Element_Array)
      is abstract;


   -------------------
   -- Push_Supplier --
   -------------------

   type Push_Supplier is abstract tagged limited private;
   type Push_Supplier_Ref is access all Push_Supplier'Class;

   procedure Disconnect
     (Supplier : access Push_Supplier)
      is abstract;


   -------------------
   -- Pull_Consumer --
   -------------------

   type Pull_Consumer is abstract tagged limited private;
   type Pull_Consumer_Ref is access all Pull_Consumer'Class;

   procedure Disconnect
     (Consumer : access Pull_Consumer)
      is abstract;


   -------------------
   -- Pull_Supplier --
   -------------------

   type Pull_Supplier is abstract tagged limited private;
   type Pull_Supplier_Ref is access all Pull_Supplier'Class;

   procedure Disconnect
     (Supplier : access Pull_Supplier)
     is abstract;

   function Pull
     (Supplier : access Pull_Supplier)
      return Ada.Streams.Stream_Element_Array
     is abstract;

   function Try_Pull
     (Supplier : access Pull_Supplier)
      return Ada.Streams.Stream_Element_Array
     is abstract;

private

   type Push_Consumer is abstract tagged limited null record;
   type Push_Supplier is abstract tagged limited null record;
   type Pull_Consumer is abstract tagged limited null record;
   type Pull_Supplier is abstract tagged limited null record;

end GLADE.Event_Communication.Interface;
