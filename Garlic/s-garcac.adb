--
--  $Id$
--

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
