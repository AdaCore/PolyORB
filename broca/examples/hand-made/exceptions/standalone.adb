
with Text_IO; use Text_IO;
with CORBA; use CORBA;
with Myexceptions;  use Myexceptions;
with Broca.Exceptions;  use Broca.Exceptions;

procedure Standalone is

   Tm, Tm2, After : Toto_Members;
   
   procedure Simple_Test (Name : String) is
      Tm: Toto_Members;
   begin
      Put ("--- raising and catching a simple exception in task " & Name & " ---");
      New_Line;
      Tm.I := 1;
      User_Raise_Exception(Toto'Identity, Tm);
   exception
      when E : Toto =>
	 New_Line;
	 Get_Members(E, After);
	 Put("toto caught in " & Name
	     & " i="
	     & Long'Image(After.I));
	 New_Line;
	 New_Line;
   end Simple_Test;

   procedure Do_Tasking is
      task A;
      task body A is
      begin
	 Simple_Test("A");
      end;
   begin
      Simple_Test("B");
   end Do_Tasking;

   
begin

   Simple_Test("main");
   
   Put ("--- raising 2 exceptions in the same task ---");
   New_Line;
   begin
      Tm.I := 3;
      User_Raise_Exception(Toto'Identity, Tm);
   exception
      when E3 : Toto =>
	 Put("toto e3 caught");
	 New_Line;
	 begin
	    Tm2.I := 4;
	    User_Raise_Exception(Toto'Identity, Tm2);
	 exception
	    when E4 : Toto =>
	       Put("toto e4 caught");
	       New_Line;
	       Get_Members(E4, After);
	       Put("e4's member : " & Long'Image(After.I));
	       New_Line;
	       New_Line;
	 end;
   end;

   Put ("--- and now some tasking ---");
   New_Line;
   Do_Tasking;
   
end Standalone;

