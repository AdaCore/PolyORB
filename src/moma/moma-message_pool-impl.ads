package MOMA.Message_Pool.Impl is

   function Publish (Message : in PolyORB.Types.String)
                     return PolyORB.Types.String;

   function Get (Message_Id : in PolyORB.Types.String)
                 return PolyORB.Types.String;

end MOMA.Message_Pool.Impl;
