type My_Object is
  tagged limited private;

procedure Op_1 (Self: in My_Object);
function Op_2 (Self : in My_Object;
               I : in Integer)
  return Boolean;

type My_Ref is
  access all My_Object'Class;
