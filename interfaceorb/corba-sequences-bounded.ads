--
-- This package provides the definitions required by the IDL-to-Ada
-- mapping specification for bounded sequences.
-- This package is instantiated for each IDL bounded sequence type.
-- This package defines the sequence type and the operations upon it.
-- This package is modeled after Ada.Strings.
--
-- Most query operations are not usable until the sequence object has
-- been initialized through an assignment.
--
-- Value semantics apply to assignment, that is, assignment of a sequence
-- value to a sequence object yields a copy of the value.
--
-- The exception INDEX_ERROR is raised when indexes are not in the range
-- of the object being manipulated.
--
-- The exception CONSTRAINT_ERROR is raised when objects that have not
-- been initialized or assigned to are manipulated.
--
-------------------------------------------------------------------

generic

    type Element is private;
    Max : Positive;    -- Maximum length of the bounded sequence

package Corba.Sequences.Bounded is

    Max_Length : constant Positive := Max;

    type Element_Array is array (Positive range <>) of Element;

    Null_Element_Array : Element_Array (1 .. 0);

    type Sequence is private;

    Null_Sequence : constant Sequence;

    subtype Length_Range is Natural range 0 .. Max_Length;

    function Length (Source : in Sequence) return Length_Range;

    type Element_Array_Access is access all Element_Array;
    procedure Free (X : in out Element_Array_Access);

    --------------------------------------------------------
    -- Conversion, Concatenation, and Selection Functions --
    --------------------------------------------------------

    function To_Sequence(Source : in Element_Array ;
                         Drop : in Truncation := Error)
                         return Sequence;

    function To_Sequence (Length : in Length_Range)
                          return Sequence;

    function To_Element_Array (Source : in Sequence)
                               return Element_Array;

    function Append (Left, Right : in Sequence ;
                     Drop : in Truncation := Error)
                     return Sequence;

    function Append (Left : in Sequence;
                     Right : in Element_Array;
                     Drop : in Truncation := Error) return Sequence;

    function Append (Left : in Element_Array;
                     Right : in Sequence;
                     Drop : in Truncation := Error) return Sequence;

    function Append (Left : in Sequence;
                     Right : in Element;
                     Drop : in Truncation := Error) return Sequence;

    function Append (Left : in Element;
                     Right : in Sequence;
                     Drop : in Truncation := Error) return Sequence;

    procedure Append (Source : in out Sequence;
                      New_Item : in Sequence;
                      Drop : in Truncation := Error);

    procedure Append (Source : in out Sequence;
                      New_Item : in Element_Array;
                      Drop : in Truncation := Error);

    procedure Append (Source : in out Sequence;
                      New_Item : in Element;
                      Drop : in Truncation := Error);

    function "&" (Left, Right : in Sequence) return Sequence;

    function "&" (Left : in Sequence;
                  Right : in Element_Array) return Sequence;

    function "&" (Left : in Element_Array;
                  Right : in Sequence) return Sequence;

    function "&" (Left : in Sequence;
                  Right : in Element) return Sequence;

    function "&" (Left : in Element;
                  Right : in Sequence) return Sequence;

    function Element_Of(Source : in Sequence;
                        Index : in Positive) return Element;

    procedure Replace_Element (Source : in out Sequence;
                               Index : in Positive;
                               By : in Element);

    function Slice (Source : in Sequence;
                    Low : in Positive; High : in Natural)
                    return Element_Array;


    function "=" (Left, Right : in Sequence) return Boolean;

    function "=" (Left : in Sequence;
                  Right : in Element_Array) return Boolean;

    function "=" (Left : in Element_Array;
                  Right : in Sequence) return Boolean;

    ----------------------
    -- Search functions --
    ----------------------

    function Index (Source : in Sequence;
                    Pattern : in Element_Array;
                    Going : in Direction := Forward) return Natural;

    function Count (Source : in Sequence;
                    Pattern : in Element_Array)
                    return Natural;

    -----------------------------------------
    -- Sequence transformation subprograms --
    -----------------------------------------

    function Replace_Slice (Source : in Sequence;
                            Low : in Positive;
                            High : in Natural;
                            By : in Element_Array;
                            Drop : in Truncation := Error) return Sequence;

    procedure Replace_Slice (Source : in out Sequence;
                             Low : in Positive;
                             High : in Natural;
                             By : in Element_Array;
                             Drop : in Truncation := Error);

    function Insert (Source : in Sequence;
                     Before : in Positive;
                     New_Item : in Element_Array;
                     Drop : in Truncation := Error) return Sequence;

    procedure Insert (Source : in out Sequence;
                      Before : in Positive;
                      New_Item : in Element_Array;
                      Drop : in Truncation := Error);

    function Overwrite (Source : in Sequence;
                        Position : in Positive;
                        New_Item : in Element_Array;
                        Drop : in Truncation := Error) return Sequence;

    procedure Overwrite (Source : in out Sequence;
                         Position : in Positive;
                         New_Item : in Element_Array;
                         Drop : in Truncation := Error);

    function Delete (Source : in Sequence;
                     From : in Positive;
                     Through : in Natural) return Sequence;

    procedure Delete (Source : in out Sequence;
                      From : in Positive;
                      Through : in Natural);

    -----------------------------------
    -- Sequence selector subprograms --
    -----------------------------------

    function Head (Source : in Sequence;
                   Count : in Natural;
                   Pad : in Element;
                   Drop : in Truncation := Error) return Sequence;

    procedure Head (Source : in out Sequence;
                    Count : in Natural;
                    Pad : in Element;
                    Drop : in Truncation := Error);

    function Tail (Source : in Sequence;
                   Count : in Natural;
                   Pad : in Element;
                   Drop : in Truncation := Error) return Sequence;

    procedure Tail (Source : in out Sequence;
                    Count : in Natural;
                    Pad : in Element;
                    Drop : in Truncation := Error);

    --------------------------------------
    -- Sequence constructor subprograms --
    --------------------------------------

    function "*" (Left : in Natural; Right : in Element) return Sequence;

    function "*" (Left : in Natural; Right : in Element_Array) return Sequence;

    function "*" (Left : in Natural; Right : in Sequence) return Sequence;

    function Replicate (Count : in Natural;
                        Item : in Element;
                        Drop : in Truncation := Error) return Sequence;

    function Replicate (Count : in Natural;
                        Item : in Element_Array;
                        Drop : in Truncation := Error) return Sequence;

    function Replicate (Count : in Natural;
                        Item : in Sequence;
                        Drop : in Truncation := Error) return Sequence;

private

    type Sequence is
        record
            Length : Length_Range := 0;
            Content : Element_Array (1 .. Max_Length);
        end record;

    Default : Sequence;

    Null_Sequence : constant Sequence := Default;

end Corba.Sequences.Bounded;

