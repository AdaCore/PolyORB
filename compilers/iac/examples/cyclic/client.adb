with Ada.Command_Line;
with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Exceptions;  use Ada.Exceptions;

with CORBA; use CORBA;
with CORBA.ORB;

with Cyclic_M.Cyclic_I;    use Cyclic_M.Cyclic_I;
with Cyclic_M.Test_Cyclic; use Cyclic_M.Test_Cyclic;

with PolyORB.Utils.Report;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

procedure Client is

   use PolyORB.Utils.Report;

   IOR : CORBA.String;

   Ref_1 : Cyclic_M.Cyclic_I.Ref;
   Ref_2 : Cyclic_M.Test_Cyclic.Ref;

   Ok : Boolean;

begin
   CORBA.ORB.Initialize ("ORB");

   if Ada.Command_Line.Argument_Count /= 2 then
      Put_Line ("usage : client <IOR_String_1> <IOR_String_2> ");
      return;
   end if;

   New_Test ("CORBA Helpers Dependencies");

   --  Getting the references

   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));
   CORBA.ORB.String_To_Object (IOR, Ref_1);

   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (2));
   CORBA.ORB.String_To_Object (IOR, Ref_2);

   --  Checking if it worked

   if Cyclic_M.Cyclic_I.Is_Nil (Ref_1) then
      Put_Line ("main : cannot invoke on a nil reference 1");
      return;
   end if;

   if Cyclic_M.Test_Cyclic.Is_Nil (Ref_2) then
      Put_Line ("main : cannot invoke on a nil reference 2");
      return;
   end if;

   Output ("test not nil reference 1", not Is_Nil (Ref_1));
   Output ("test not nil reference 2", not Is_Nil (Ref_2));

   declare
      use Cyclic_M.Test_Cyclic.Convert_Forward;

      X  : Cyclic_M.Test_Cyclic.Ref;
   begin
      Ok := (X =
             From_Forward
             (echoForwardStruct
              (Ref_1,
               (F1 => To_Forward (X))).F1));
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
   end;
   Output ("test echoForwardStruct", Ok);

   declare
      use IDL_SEQUENCE_10_Cyclic_M_Cyclic_I_SimpleType2;

      Seq : ShortSeq := ShortSeq (Null_Sequence);
   begin
      Seq := Seq & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10;
      Output ("test bounded sequence", echoShortSeq (Ref_2, Seq) = Seq);
   end;

   End_Report;
end Client;
