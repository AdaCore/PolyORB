with Test_Strings; use Test_Strings;
with Sequences;    use Sequences;
with Ada.Text_IO;
with Report; use Report;
with Ada.Unchecked_Conversion;

procedure Test_UnbSeq is

   S1, S2, S3 : Sequence;
   EA1        : Element_Array := ('1', '2', '3');
   EA2        : Element_Array := ('X', 'Y', 'Z');

   function To_String (EA : Element_Array) return String is
      subtype T1 is Element_Array (EA'Range);
      subtype T2 is String (EA'Range);
      function T1_To_T2 is new Ada.Unchecked_Conversion (T1, T2);
   begin
      return T1_To_T2 (EA);
   end To_String;

begin
   Append (S1, 'A');
   Append (S1, 'B');
   Append (S1, 'C');
   Append (S1, EA1);
   Append (S2, EA2);
   Append (S1, S2);
   S3 := '~' & S1 & To_Element_Array (S2) & EA1;
   Output ("Test 01", (S3 = To_Sequence (To_Element_Array (S3))));
   Output ("Test 02", (Element_Of (S3, 1) = '~'));
   Output ("Test 03", (Element_Of (S3, Length (S3)) = '3'));
   Replace_Element (S3, 1, 'A');
   Output ("Test 04", (Slice (S3, 2, 4) = ('A', 'B', 'C')));
   Output ("Test 05", (Index (S3, ('A', 'B'), Forward) =
                       Index (S3, ('A', 'B'), Backward)));
   Output ("Test 06", (Count (S3, ('1', '2')) = 2));
   declare
      Low  : Natural := Index (S3, ('A', 'B'), Forward);
      High : Natural := Low + 1;
      By   : Element_Array := ('1', '2', '1', '2');
   begin
      Replace_Slice (S3, Low, High, By);
      Output ("Test 07", (Count (S3, ('1', '2')) = 4));
      Insert (S3, Low, (1 => 'Z'));
      Insert (S3, Low, ('X', 'Y'));
      Output ("Test 08", "AXYZ1212C123XYZXYZ123" =
              To_String (To_Element_Array (S3)));
      Low := Index (S3, (1 => 'X'), Backward);
      Overwrite (S3, Low, To_Element_Array (20 * To_Sequence ((1 => '-'))));
      Delete (S3, 1, Low - 1);
      Head   (S3, 40, '+');
      Tail   (S3, 30, '=');
      Output ("Test 09", (S3 = (10 * '-') & (20 * '+')));
   end;
   Delete (S3, 1, Length (S3));
   Output ("Test 10", (S3 = Null_Sequence));
   Output ("Test 11",
           ("" = To_String (To_Element_Array (To_Sequence ((1 .. 0 => ' '))))));
end Test_UnbSeq;


