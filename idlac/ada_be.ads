--  with Tree;


package Ada_Be is

   type N_Root_Acc is tagged null record;

   --  takes a parse tree as input and generates the necessary files
   procedure Generate_Code (Root : in N_Root_Acc);

end Ada_Be;

