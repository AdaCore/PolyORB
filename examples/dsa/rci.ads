with RT;

package RCI is

   pragma Remote_Call_Interface;
   --  pragma All_Calls_Remote;

   type Color is (Red, Green, Blue);

   type Hue is new Color;
   --  Expanded to: type hueB is new Color; subtype hue is hueB;

   subtype Tint is Color;

   type Predicate is new Boolean;
   subtype Predicate2 is Predicate;

   type Predicate3 is new Predicate;
   subtype Predicate4 is Predicate3;

   type Trit is range 0 .. 2;

   procedure My_Proc (X : in Integer; Y : in out Predicate; Z : out Trit);

   function My_Func (S : String) return Color;

   function Get_Obj (Name : String) return RT.RACW;

   function echoString (S : String) return String;

end RCI;
