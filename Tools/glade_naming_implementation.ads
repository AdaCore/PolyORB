------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--          G L A D E _ N A M I N G _ I M P L E M E N T A T I O N           --
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

with GLADE.Event_Communication.Interface;
use GLADE.Event_Communication.Interface;
with GLADE.Event_Channel_Admin.Interface;
use GLADE.Event_Channel_Admin.Interface;

package GLADE_Naming_Implementation is

   pragma Remote_Call_Interface;

   type Binding_Kind is
      (Channel_K,
       Push_Supp_K,
       Push_Cons_K,
       Pull_Supp_K,
       Pull_Cons_K);

   type Binding_Type (K : Binding_Kind := Channel_K) is
      record
         case K is
            when Channel_K   =>
               Channel   : Event_Channel_Ref;
            when Push_Supp_K =>
               Push_Supp : Push_Supplier_Ref;
            when Pull_Supp_K =>
               Pull_Supp : Pull_Supplier_Ref;
            when Push_Cons_K =>
               Push_Cons : Push_Consumer_Ref;
            when Pull_Cons_K =>
               Pull_Cons : Pull_Consumer_Ref;
         end case;
      end record;

   procedure Bind (N : String; B : Binding_Type);
   function Resolve (N : String) return Binding_Type;

   Already_Bound : exception;
   Not_Found     : exception;

end GLADE_Naming_Implementation;
