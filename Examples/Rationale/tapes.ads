package Tapes is

   pragma Pure(Tapes);

   type Tape is abstract tagged limited private;

   -- Primitive dispatching operations where
   -- Tape is controlling operand

   procedure Copy (From, To : access Tape; Num_Recs : in Natural) is abstract;

   procedure Rewind (T : access Tape) is abstract;

private
   type Tape is abstract tagged limited null record;
end Tapes;
