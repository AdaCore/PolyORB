with System.Garlic.Utils;

package System.Garlic.Table is

   generic
      type Index_Type     is range <>;
      Initial_Size   : Positive;
      Increment_Size : Positive;

      type Component_Type is private;
      Null_Component : Component_Type;

      type Parameter_Type is private;

   package Concurrent is

      Null_Index : constant Index_Type := Index_Type'First;

      --  These procedures are task-safe.

      function  Allocate (N : Positive := 1) return Index_Type;

      function  Get (S : String) return Index_Type;
      function  Get (N : Index_Type) return String;
      --  Associate a name to an index.

      function  Get (N : Index_Type) return Component_Type;
      procedure Set (N : Index_Type; C : Component_Type);
      --  Associate an index to a component.

      type Process_Type is access procedure
        (N         : in Index_Type;
         Parameter : in Parameter_Type;
         Component : in out Component_Type;
         Status    : out Utils.Status_Type);
      --  Procedure to execute into a critical section. This procedure
      --  applies to Component and takes a Parameter. If Status returns the
      --  result of this procedure on Component (Modified or
      --  Unmodified). If Status is Postponed, this means that the request
      --  has been postponed and should be re-executed when Component value
      --  has been modified.

      procedure Apply
        (N         : in Index_Type;
         Parameter : in Parameter_Type;
         Process   : in Process_Type);
      --  Apply Process to Component of index N. Use Parameter as parameter
      --  of Process.

   end Concurrent;

   generic
      type Index_Type     is range <>;
      Initial_Size   : Positive;
      Increment_Size : Positive;

      type Component_Type is private;
      Null_Component : Component_Type;

   package Sequential is

      Null_Index : constant Index_Type := Index_Type'First;

      function  Allocate (N : Positive := 1) return Index_Type;

      function  Get (N : Index_Type) return Component_Type;
      procedure Set (N : Index_Type; C : Component_Type);

   end Sequential;

end System.Garlic.Table;
