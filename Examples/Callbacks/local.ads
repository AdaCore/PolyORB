with Common; use Common;
with Types;  use Types;
package Local is
   pragma Remote_Types;

   type New_Worker is new Worker with private;

   procedure Work
     (W : access New_Worker;
      Q : Query;
      C : Callback);

   procedure Initialize;

private
   type New_Worker is new Worker with null record;
end Local;
