------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--               S Y S T E M . G A R L I C . C A C H I N G                  --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

package body System.Garlic.Caching is

   Data_Map : array (Index_Type) of Data_Type := (others => Unset);
   Queried  : array (Index_Type) of Boolean   := (others => False);

   Dead : Boolean := False;

   protected Keeper is
      entry Get (Index_Type) (Value : out Data_Type);
      procedure Set (Index : in Index_Type; Value : in Data_Type);
      procedure Die;
   end Keeper;
   --  This keeper will use global variables in the guards, but Set will
   --  always be called then, causing the guards to be reevaluated.

   ---------
   -- Die --
   ---------

   procedure Die is
   begin
      Keeper.Die;
   end Die;

   ---------
   -- Get --
   ---------

   function Get (Index : Index_Type) return Data_Type is
      Value : Data_Type := Data_Map (Index);
   begin
      if Value = Unset then
         Keeper.Get (Index) (Value);
      end if;
      if Dead then
         raise Program_Error;
      end if;
      return Value;
   end Get;

   ------------
   -- Keeper --
   ------------

   protected body Keeper is

      ---------
      -- Die --
      ---------

      procedure Die is
      begin
         Dead := True;
      end Die;

      ---------
      -- Get --
      ---------

      entry Get (for Index in Index_Type) (Value : out Data_Type)
      when
        Data_Map (Index) /= Unset or else
        not Queried (Index) or else
        Dead is
      begin
         Value := Data_Map (Index);
         if Value = Unset then
            Queried (Index) := True;
         end if;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (Index : in Index_Type; Value : in Data_Type) is
      begin
         Data_Map (Index) := Value;
      end Set;

   end Keeper;

   ---------
   -- Set --
   ---------

   procedure Set (Index : in Index_Type; Value : in Data_Type) is
   begin
      Keeper.Set (Index, Value);
   end Set;

end System.Garlic.Caching;
