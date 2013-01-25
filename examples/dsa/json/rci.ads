with RT_JSON;
package RCI is
   pragma Remote_Call_Interface;

   function Frob (X : RT_JSON.JSON_Wrapper) return RT_JSON.JSON_Wrapper;

end RCI;
