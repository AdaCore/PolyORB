package Prime_2 is
   pragma Remote_Call_Interface;

   procedure Test_Number
     (Number  : in     Natural;
      Cell    : in out Natural;
      Prime   : out    Natural);

end Prime_2;
