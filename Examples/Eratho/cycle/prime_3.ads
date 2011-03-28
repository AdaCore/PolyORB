with Common; use Common;

package Prime_3 is
   pragma Remote_Call_Interface;

   procedure Test_Primarity (Number  : in  Natural);
   pragma Asynchronous (Test_Primarity);

end Prime_3;
