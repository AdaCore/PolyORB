package Corba.Sequences is

-----------------------------------------------------------------------
--
-- CORBA.Sequences is the parent of the bounded and unbounded sequence
-- packages.  Some exceptions and types common to both are declared here
-- (following the structure of Ada.Strings).
--
-- Length_Error is raised when sequence lengths are exceeded.
-- Pattern_Error is raised when a null pattern string is passed.
-- Index_Error is raised when indexes are out of range.
--
-----------------------------------------------------------------------

    Length_Error, Pattern_Error, Index_Error : exception;

    type Alignment is (Left, Right, Center);
    type Truncation is (Left, Right, Error);
    type Membership is (Inside, Outside);
    type Direction is (Forward, Backward);

    type Trim_End is (Left, Right, Both);

end Corba.Sequences;

