---------------------------------------------------------------------
---                                                               ---
---       AdaBroker : package body corba.sequences.bounded        ---
---                                                               ---
---  The implementation is the one provided by the Gnack project  ---
---                                                               ---
---------------------------------------------------------------------

with Ada.Unchecked_Deallocation;


package body Corba.Sequences.Unbounded is

    function Length (Source : in Sequence) return Natural is
    begin
        return Source.Length;
    end Length;

----------------------------------------------------------------------

    procedure Free (X : in out Element_Array_Access) is
        procedure Deallocate is new Ada.Unchecked_Deallocation
                                       (Element_Array, Element_Array_Access);
    begin
        Deallocate (X);
    end Free;

----------------------------------------------------------------------

    --------------------------------------------------------
    -- Conversion, Concatenation, and Selection functions --
    --------------------------------------------------------

    function To_Sequence (Source : in Element_Array) return Sequence is

        Source_Length : constant Natural := Source'Length;
        Result : Sequence;

    begin

        Result.Content := new Element_Array (1 .. Source_Length);

        Result.Content (1 .. Source_Length) := Source;
        Result.Length := Source_Length;

        return Result;

    end To_Sequence;

----------------------------------------------------------------------

    function To_Sequence (Length : in Natural) return Sequence is

        Result : Sequence;

    begin

        Result.Content := new Element_Array (1 .. Length);

        Result.Content (1 .. Length) := Null_Element_Array;
        Result.Length := Length;

        return Result;

    end To_Sequence;

----------------------------------------------------------------------

    function To_Element_Array (Source : in Sequence) return Element_Array is
    begin

        return Source.Content (1 .. Source.Length);

    end To_Element_Array;

----------------------------------------------------------------------

    procedure Append (Source : in out Sequence; New_Item : in Sequence) is

        Total_Length : constant Natural := Source.Length + New_Item.Length;
        Temp : Element_Array_Access;

    begin

        if New_Item.Length = 0 then

            -- no need to add an empty sequence
            null;

        elsif Source.Length = 0 then

            if Source.Content = null then

                -- there is no source

                Source.Content := new Element_Array (1 .. Total_Length);

                Source.Content (1 .. Total_Length) :=
                   New_Item.Content (1 .. Total_Length);

            elsif Source.Content'Length < New_Item.Length then

                -- source is too small

                Temp := new Element_Array (1 .. Total_Length);

                Temp (1 .. Total_Length) :=
                   New_Item.Content (1 .. Total_Length);

                Free (Source.Content);

                Source.Content := Temp;

            else

                Source.Content (1 .. Total_Length) :=
                   New_Item.Content (1 .. Total_Length);

            end if;
            Source.Length := Total_Length;

        elsif Total_Length <= Source.Content'Length then

            Source.Content (Source.Length + 1 .. Total_Length) :=
               New_Item.Content (1 .. New_Item.Length);
            Source.Length := Total_Length;

        else

            -- source is too small

            Temp := new Element_Array (1 .. Total_Length);

            Temp (1 .. Source.Length) := Source.Content (1 .. Source.Length);
            Temp (Source.Length + 1 .. Total_Length) :=
               New_Item.Content (1 .. New_Item.Length);
            Free (Source.Content);
            Source.Content := Temp;
            Source.Length := Total_Length;

        end if;

    end Append;

----------------------------------------------------------------------

    procedure Append (Source : in out Sequence; New_Item : in Element_Array) is

        Total_Length : constant Natural := Source.Length + New_Item'Length;
        Temp : Element_Array_Access;

    begin

        if New_Item'Length = 0 then

            -- no need to add an empty sequence
            null;

        elsif Source.Length = 0 then

            if Source.Content = null then

                -- there is no source

                Source.Content := new Element_Array (1 .. Total_Length);

                Source.Content (1 .. Total_Length) := New_Item;

            elsif Source.Content'Length < New_Item'Length then

                -- source is too small

                Temp := new Element_Array (1 .. Total_Length);

                Temp (1 .. Total_Length) := New_Item;
                Free (Source.Content);
                Source.Content := Temp;

            else

                Source.Content (1 .. Total_Length) := New_Item;

            end if;
            Source.Length := Total_Length;

        elsif Total_Length <= Source.Content'Length then

            Source.Content (Source.Length + 1 .. Total_Length) := New_Item;
            Source.Length := Total_Length;

        else

            -- source is too small

            Temp := new Element_Array (1 .. Total_Length);

            Temp (1 .. Source.Length) := Source.Content (1 .. Source.Length);
            Temp (Source.Length + 1 .. Total_Length) := New_Item;
            Free (Source.Content);
            Source.Content := Temp;
            Source.Length := Total_Length;

        end if;

    end Append;

----------------------------------------------------------------------

    procedure Append (Source : in out Sequence; New_Item : in Element) is

        Total_Length : constant Natural := Source.Length + 1;
        Temp : Element_Array_Access;

    begin

        if Source.Length = 0 then

            if Source.Content = null then

                -- there is no source

                Source.Content := new Element_Array (1 .. Total_Length);

            end if;
            Source.Content (1) := New_Item;
            Source.Length := Total_Length;

        elsif Total_Length <= Source.Content'Length then

            Source.Content (Total_Length) := New_Item;
            Source.Length := Total_Length;

        else

            -- source is too small

            Temp := new Element_Array (1 .. Total_Length);

            Temp (1 .. Source.Length) := Source.Content (1 .. Source.Length);
            Temp (Total_Length) := New_Item;
            Free (Source.Content);
            Source.Content := Temp;
            Source.Length := Total_Length;

        end if;

    end Append;

----------------------------------------------------------------------

    function "&" (Left, Right : in Sequence) return Sequence is

        Left_Length : constant Natural := Left.Length;
        Right_Length : constant Natural := Right.Length;
        Total_Length : constant Natural := Left_Length + Right_Length;
        Result : Sequence;

    begin

        Result.Content := new Element_Array (1 .. Total_Length);

        Result.Content (1 .. Left_Length) := Left.Content (1 .. Left_Length);
        Result.Content (Left_Length + 1 .. Total_Length) :=
           Right.Content (1 .. Right_Length);
        Result.Length := Total_Length;

        return Result;

    end "&";

----------------------------------------------------------------------

    function "&" (Left : in Sequence; Right : in Element_Array)
                 return Sequence is

        Left_Length : constant Natural := Left.Length;
        Total_Length : constant Natural := Left_Length + Right'Length;
        Result : Sequence;

    begin

        Result.Content := new Element_Array (1 .. Total_Length);

        Result.Content (1 .. Left_Length) := Left.Content (1 .. Left_Length);
        Result.Content (Left_Length + 1 .. Total_Length) := Right;
        Result.Length := Total_Length;

        return Result;

    end "&";

----------------------------------------------------------------------

    function "&" (Left : in Element_Array; Right : in Sequence)
                 return Sequence is

        Left_Length : constant Natural := Left'Length;
        Right_Length : constant Natural := Right.Length;
        Total_Length : constant Natural := Left_Length + Right_Length;
        Result : Sequence;

    begin

        Result.Content := new Element_Array (1 .. Total_Length);

        Result.Content (1 .. Left_Length) := Left;
        Result.Content (Left_Length + 1 .. Total_Length) :=
           Right.Content (1 .. Right_Length);
        Result.Length := Total_Length;

        return Result;

    end "&";

----------------------------------------------------------------------

    function "&" (Left : in Sequence; Right : in Element) return Sequence is

        Left_Length : constant Natural := Left.Length;
        Total_Length : constant Natural := Left_Length + 1;
        Result : Sequence;

    begin

        Result.Content := new Element_Array (1 .. Total_Length);

        Result.Content (1 .. Left_Length) := Left.Content (1 .. Left_Length);
        Result.Content (Total_Length) := Right;
        Result.Length := Total_Length;

        return Result;

    end "&";

----------------------------------------------------------------------

    function "&" (Left : in Element; Right : in Sequence) return Sequence is

        Right_Length : constant Natural := Right.Length;
        Total_Length : constant Natural := Right_Length + 1;
        Result : Sequence;

    begin

        Result.Content := new Element_Array (1 .. Total_Length);

        Result.Content (1) := Left;
        Result.Content (2 .. Result.Length) :=
           Right.Content (1 .. Right_Length);
        Result.Length := Total_Length;

        return Result;

    end "&";

----------------------------------------------------------------------

    function Element_Of
                (Source : in Sequence; Index : in Positive) return Element is
    begin

        if Index <= Source.Length then

            return Source.Content (Index);

        else

            raise Index_Error;

        end if;

    end Element_Of;

----------------------------------------------------------------------

    procedure Replace_Element (Source : in out Sequence;
                               Index : in Positive;
                               By : in Element) is
    begin

        if Index <= Source.Length then

            Source.Content (Index) := By;

        else

            raise Index_Error;

        end if;

    end Replace_Element;

----------------------------------------------------------------------

    function Slice (Source : in Sequence; Low : in Positive; High : in Natural)
                   return Element_Array is
    begin

        if Low <= Source.Length and then High <= Source.Length then

            return Source.Content (Low .. High);

        else

            raise Index_Error;

        end if;

    end Slice;

----------------------------------------------------------------------

    function "=" (Left, Right : in Sequence) return Boolean is
    begin

        return Left.Length = Right.Length and then
                  Left.Content (1 .. Left.Length) =
                     Right.Content (1 .. Right.Length);

    end "=";

----------------------------------------------------------------------

    function "=" (Left : in Element_Array; Right : in Sequence)
                 return Boolean is
    begin

        return Left'Length = Right.Length and then
                  Left = Right.Content (1 .. Right.Length);

    end "=";

----------------------------------------------------------------------

    function "=" (Left : in Sequence; Right : in Element_Array)
                 return Boolean is
    begin

        return Left.Length = Right'Length and then
                  Left.Content (1 .. Left.Length) = Right;

    end "=";

----------------------------------------------------------------------

    ----------------------
    -- Search functions --
    ----------------------

    function Index (Source : in Sequence;
                    Pattern : in Element_Array;
                    Going : in Direction := Forward) return Natural is

    begin

        if Pattern /= Null_Element_Array then

            case Going is

                when Forward =>
                    for J in 1 .. Source.Length - Pattern'Length + 1 loop

                        if Source.Content (J) = Pattern (Pattern'First) and then
                           Source.Content (J .. J + Pattern'Length - 1) =
                              Pattern then

                            return J;

                        end if;

                    end loop;

                when Backward =>
                    for J in reverse 1 .. Source.Length -
                                             Pattern'Length + 1 loop

                        if Source.Content (J) = Pattern (Pattern'First) and then
                           Source.Content (J .. J + Pattern'Length - 1) =
                              Pattern then

                            return J;

                        end if;

                    end loop;

            end case;

            -- No match found yet
            return 0;

        else

            raise Pattern_Error;

        end if;

    end Index;

----------------------------------------------------------------------

    function Count (Source : in Sequence; Pattern : in Element_Array)
                   return Natural is

        N : Natural;

    begin

        if Pattern /= Null_Element_Array then

            N := 0;
            for J in 1 .. Source.Length - (Pattern'Length - 1) loop

                if Source.Content (J) = Pattern (Pattern'First) and then
                   Source.Content (J .. J + Pattern'Length - 1) = Pattern then

                    N := N + 1;

                end if;

            end loop;
            return N;

        else

            raise Pattern_Error;

        end if;

    end Count;

----------------------------------------------------------------------

    -----------------------------------------
    -- Sequence transformation subprograms --
    -----------------------------------------

    function Replace_Slice (Source : in Sequence;
                            Low : in Positive;
                            High : in Natural;
                            By : in Element_Array) return Sequence is

        Source_Length : constant Natural := Source.Length;
        By_Length : constant Integer := By'Length;
        Total_Length : constant Integer :=
           Source_Length + By_Length + Low - High - 1;
        Result : Sequence;

    begin

        if Low > Source_Length + 1 or else High > Source_Length then

            raise Index_Error;

        elsif High < Low then

            return Insert (Source => Source, Before => Low, New_Item => By);

        else

            Result.Content := new Element_Array (1 .. Total_Length);

            Result.Content (1 .. Low - 1) := Source.Content (1 .. Low - 1);
            Result.Content (Low .. Low + By_Length - 1) := By;
            Result.Content (Low + By_Length .. Total_Length) :=
               Source.Content (High + 1 .. Source_Length);
            Result.Length := Total_Length;

            return Result;

        end if;

    end Replace_Slice;

----------------------------------------------------------------------

    procedure Replace_Slice (Source : in out Sequence;
                             Low : in Positive;
                             High : in Natural;
                             By : in Element_Array) is

        Source_Length : constant Natural := Source.Length;
        By_Length : constant Integer := By'Length;
        Total_Length : constant Integer :=
           Source_Length + By_Length + Low - High - 1;
        Temp : Element_Array_Access;

    begin

        if Low > Source_Length + 1 or else High > Source_Length then

            raise Index_Error;

        elsif High < Low then

            Insert (Source => Source, Before => Low, New_Item => By);

        elsif Total_Length > Source.Content'Length then

            Temp := new Element_Array (1 .. Total_Length);

            Temp (1 .. Low - 1) := Source.Content (1 .. Low - 1);
            Temp (Low .. Low + By_Length - 1) := By;
            Temp (Low + By_Length .. Total_Length) :=
               Source.Content (High + 1 .. Source_Length);
            Free (Source.Content);
            Source.Content := Temp;
            Source.Length := Total_Length;

        else

            Source.Content (Low + By_Length .. Total_Length) :=
               Source.Content (High + 1 .. Source_Length);
            Source.Content (Low .. Low + By_Length - 1) := By;
            Source.Length := Total_Length;

        end if;

    end Replace_Slice;

----------------------------------------------------------------------

    function Insert (Source : in Sequence;
                     Before : in Positive;
                     New_Item : in Element_Array) return Sequence is

        Source_Length : constant Natural := Source.Length;
        New_Length : constant Natural := New_Item'Length;
        Total_Length : constant Natural := Source_Length + New_Length;
        Result : Sequence;

    begin

        if Source.Length + 1 < Before then

            raise Index_Error;

        else

            Result.Content := new Element_Array (1 .. Total_Length);

            if Source.Length = 0 then

                Result.Content (1 .. Total_Length) := New_Item;

            else

                Result.Content (1 .. Before - 1) :=
                   Source.Content (1 .. Before - 1);
                Result.Content (Before .. Before + New_Length - 1) := New_Item;
                Result.Content (Before + New_Length .. Total_Length) :=
                   Source.Content (Before .. Source_Length);

            end if;
            Result.Length := Total_Length;

            return Result;

        end if;

    end Insert;

----------------------------------------------------------------------

    procedure Insert (Source : in out Sequence;
                      Before : in Positive;
                      New_Item : in Element_Array) is

        Source_Length : constant Natural := Source.Length;
        New_Length : constant Natural := New_Item'Length;
        Total_Length : constant Natural := Source_Length + New_Length;
        Temp : Element_Array_Access;

    begin

        if Source.Length + 1 < Before then

            raise Index_Error;

        elsif New_Length > 0 then

            if Source.Length = 0 then

                if Source.Content = null then

                    Source.Content := new Element_Array (1 .. Total_Length);

                elsif Source.Content'Length < Total_Length then

                    Free (Source.Content);
                    Source.Content := new Element_Array (1 .. Total_Length);

                end if;
                Source.Content (1 .. Total_Length) := New_Item;
                Source.Length := Total_Length;

            elsif Source.Content'Length < Total_Length then

                Temp := new Element_Array (1 .. Total_Length);

                Temp (1 .. Before - 1) := Source.Content (1 .. Before - 1);
                Temp (Before .. Before + New_Length - 1) := New_Item;
                Temp (Before + New_Length .. Total_Length) :=
                   Source.Content (Before .. Source.Length);
                Free (Source.Content);
                Source.Content := Temp;
                Source.Length := Total_Length;

            else

                Source.Content (Before + New_Length .. Total_Length) :=
                   Source.Content (Before .. Source_Length);
                Source.Content (Before .. Before + New_Length - 1) := New_Item;
                Source.Length := Total_Length;

            end if;

        end if;

    end Insert;

----------------------------------------------------------------------

    function Overwrite (Source : in Sequence;
                        Position : in Positive;
                        New_Item : in Element_Array) return Sequence is

        Source_Length : constant Natural := Source.Length;
        Endpos : constant Natural := Position + New_Item'Length - 1;
        Result : Sequence;

    begin

        if Position > Source_Length + 1 then

            raise Index_Error;

        else

            if Endpos >= Source_Length then

                Result.Content := new Element_Array (1 .. Endpos);

                Result.Content (1 .. Position - 1) :=
                   Source.Content (1 .. Position - 1);
                Result.Content (Position .. Endpos) := New_Item;
                Result.Length := Endpos;

            else

                Result.Content := new Element_Array (1 .. Source_Length);

                Result.Content (1 .. Position - 1) :=
                   Source.Content (1 .. Position - 1);
                Result.Content (Position .. Endpos) := New_Item;
                Result.Content (Endpos + 1 .. Source.Length) :=
                   Source.Content (Endpos + 1 .. Source_Length);
                Result.Length := Source_Length;

            end if;

            return Result;

        end if;

    end Overwrite;

----------------------------------------------------------------------

    procedure Overwrite (Source : in out Sequence;
                         Position : in Positive;
                         New_Item : in Element_Array) is

        Source_Length : constant Natural := Source.Length;
        Endpos : constant Natural := Position + New_Item'Length - 1;
        Temp : Element_Array_Access;

    begin

        if Position > Source_Length + 1 then

            raise Index_Error;

        else

            if Source.Length = 0 then

                if Source.Content = null then

                    Source.Content := new Element_Array (1 .. Endpos);

                elsif Endpos > Source.Content'Length then

                    Free (Source.Content);
                    Source.Content := new Element_Array (1 .. Endpos);

                else

                    null;

                end if;
                Source.Content (1 .. Endpos) := New_Item;
                Source.Length := Endpos;

            elsif Endpos > Source.Content'Length then

                Temp := new Element_Array (1 .. Endpos);

                Temp (1 .. Position - 1) := Source.Content (1 .. Position - 1);
                Temp (Position .. Endpos) := New_Item;
                Free (Source.Content);
                Source.Content := Temp;
                Source.Length := Endpos;

            else

                Source.Content (Position .. Endpos) := New_Item;
                if Endpos > Source.Length then

                    Source.Length := Endpos;

                end if;

            end if;

        end if;

    end Overwrite;

----------------------------------------------------------------------

    function Delete (Source : in Sequence;
                     From : in Positive;
                     Through : in Natural) return Sequence is

        Source_Length : constant Natural := Source.Length;
        Num_Delete : constant Integer := Through - From + 1;
        Total_Length : constant Integer := Source_Length - Num_Delete;
        Result : Sequence;

    begin

        if Through < From then

            Result.Content := new Element_Array (1 .. Source_Length);

            Result.Content := Source.Content;
            Result.Length := Source_Length;

            return Result;

        elsif From > Source_Length + 1 then

            raise Index_Error;

        elsif Through > Source_Length then

            raise Index_Error;

        else

            Result.Content := new Element_Array (1 .. Total_Length);

            Result.Content (1 .. From - 1) := Source.Content (1 .. From - 1);
            Result.Content (From .. Total_Length) :=
               Source.Content (Through + 1 .. Source_Length);
            Result.Length := Total_Length;

            return Result;

        end if;

    end Delete;

----------------------------------------------------------------------

    procedure Delete (Source : in out Sequence;
                      From : in Positive;
                      Through : in Natural) is

        Source_Length : constant Natural := Source.Length;
        Num_Delete : constant Integer := Through - From + 1;
        Total_Length : constant Integer := Source_Length - Num_Delete;

    begin

        if Through < From then

            return;

        elsif From > Source_Length + 1 then

            raise Index_Error;

        elsif Through > Source_Length then

            raise Index_Error;

        else

            Source.Length := Total_Length;
            Source.Content (From .. Total_Length) :=
               Source.Content (Through + 1 .. Source_Length);

        end if;

    end Delete;

----------------------------------------------------------------------

    -----------------------------------
    -- Sequence selector subprograms --
    -----------------------------------

    function Head (Source : in Sequence; Count : in Natural; Pad : in Element)
                  return Sequence is

        Source_Length : constant Natural := Source.Length;
        Result : Sequence;

    begin

        Result.Content := new Element_Array (1 .. Count);

        if Source_Length < Count then

            Result.Content (1 .. Source_Length) :=
               Source.Content (1 .. Source_Length);
            Result.Content (Source_Length + 1 .. Count) := (others => Pad);

        else

            Result.Content (1 .. Count) := Source.Content (1 .. Count);

        end if;
        Result.Length := Count;

        return Result;

    end Head;

----------------------------------------------------------------------

    procedure Head (Source : in out Sequence;
                    Count : in Natural;
                    Pad : in Element) is

        Source_Length : constant Natural := Source.Length;
        Npad : constant Integer := Count - Source_Length;
        Temp : Element_Array_Access;

    begin

        if Source_Length = 0 then

            if Count = 0 then

                null;

            elsif Source.Content = null then

                Source.Content := new Element_Array (1 .. Count);

            elsif Count > Source.Content'Length then

                Free (Source.Content);
                Source.Content := new Element_Array (1 .. Count);

            else

                null;

            end if;
            Source.Content (1 .. Count) := (others => Pad);
            Source.Length := Count;

        elsif Count > Source.Content'Length then

            Temp := new Element_Array (1 .. Count);
            Temp (1 .. Source_Length) := Source.Content (1 .. Source_Length);
            Temp (Source_Length + 1 .. Count) := (others => Pad);
            Free (Source.Content);
            Source.Content := Temp;
            Source.Length := Count;

        elsif Count > Source.Length then

            Source.Content (Source_Length + 1 .. Count) := (others => Pad);
            Source.Length := Count;

        else

            Source.Length := Count;

        end if;

    end Head;

----------------------------------------------------------------------

    function Tail (Source : in Sequence; Count : in Natural; Pad : in Element)
                  return Sequence is

        Source_Length : constant Natural := Source.Length;
        Result : Sequence;

    begin

        Result.Content := new Element_Array (1 .. Count);

        if Source_Length < Count then

            Result.Content (1 .. Count - Source_Length) := (others => Pad);
            Result.Content (Count - Source_Length + 1 .. Count) :=
               Source.Content (1 .. Source_Length);

        else

            Result.Content (1 .. Count) :=
               Source.Content (Source_Length - Count + 1 .. Source_Length);

        end if;
        Result.Length := Count;

        return Result;

    end Tail;

----------------------------------------------------------------------

    procedure Tail (Source : in out Sequence;
                    Count : in Natural;
                    Pad : in Element) is

        Source_Length : constant Natural := Source.Length;
        Npad : constant Integer := Count - Source_Length;
        Temp : Element_Array_Access;

    begin

        if Source_Length = 0 then

            if Count = 0 then

                null;

            elsif Source.Content = null then

                Source.Content := new Element_Array (1 .. Count);

            elsif Count > Source.Content'Length then

                Free (Source.Content);
                Source.Content := new Element_Array (1 .. Count);

            else

                null;

            end if;
            Source.Content (1 .. Count) := (others => Pad);
            Source.Length := Count;

        elsif Count > Source.Content'Length then

            Temp := new Element_Array (1 .. Count);

            Temp (Count - Source_Length + 1 .. Count) :=
               Source.Content (1 .. Source_Length);
            Temp (1 .. Count - Source_Length) := (others => Pad);
            Free (Source.Content);
            Source.Content := Temp;
            Source.Length := Count;

        elsif Count > Source_Length then

            Source.Content (Count - Source_Length + 1 .. Count) :=
               Source.Content (1 .. Source_Length);
            Source.Content (1 .. Count - Source_Length) := (others => Pad);
            Source.Length := Count;

        else

            Source.Content (1 .. Count) :=
               Source.Content (Source_Length - Count + 1 .. Source_Length);
            Source.Length := Count;

        end if;

    end Tail;

----------------------------------------------------------------------

    ------------------------------------
    -- String constructor subprograms --
    ------------------------------------

    function "*" (Left : in Natural; Right : in Element) return Sequence is

        Result : Sequence;

    begin

        Result.Content := new Element_Array (1 .. Left);

        for J in 1 .. Left loop
            Result.Content (J) := Right;
        end loop;
        Result.Length := Left;

        return Result;

    end "*";

----------------------------------------------------------------------

    function "*" (Left : in Natural; Right : in Element_Array)
                 return Sequence is

        Right_Length : constant Natural := Right'Length;
        Total_Length : constant Natural := Left * Right_Length;
        Pos : Positive := 1;
        Result : Sequence;

    begin

        if Total_Length > 0 then

            Result.Content := new Element_Array (1 .. Total_Length);

            for J in 1 .. Left loop

                Result.Content (Pos .. Pos + Right_Length - 1) := Right;
                Pos := Pos + Right_Length;

            end loop;

        end if;
        Result.Length := Total_Length;

        return Result;

    end "*";

----------------------------------------------------------------------

    function "*" (Left : in Natural; Right : in Sequence) return Sequence is

        Right_Length : constant Natural := Right.Length;
        Total_Length : constant Natural := Left * Right_Length;
        Pos : Positive := 1;
        Result : Sequence;

    begin

        if Total_Length > 0 then

            Result.Content := new Element_Array (1 .. Total_Length);

            for J in 1 .. Left loop

                Result.Content (Pos .. Pos + Right_Length - 1) :=
                   Right.Content (1 .. Right_Length);
                Pos := Pos + Right_Length;

            end loop;

        end if;
        Result.Length := Total_Length;

        return Result;

    end "*";

----------------------------------------------------------------------

    procedure Initialize (Object : in out Sequence) is
    begin
        null;
    end Initialize;


    procedure Adjust (Object : in out Sequence) is
        Temp : Element_Array_Access;
    begin
        Temp := new Element_Array (1 .. Object.Length);
        Temp (1 .. Object.Length) := Object.Content (1 .. Object.Length);
        Object.Content := Temp;
    end Adjust;


    procedure Finalize (Object : in out Sequence) is
    begin
        Free (Object.Content);
    end Finalize;


end Corba.Sequences.Unbounded;

