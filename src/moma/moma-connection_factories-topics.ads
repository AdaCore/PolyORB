package MOMA.Connection_Factories.Topics is

   type Connection_Factory_Topic is new Connection_Factory with null record;


   procedure Create (Self     : out Connection_Factory_Topic;
                     Remote   : PolyORB.References.Ref);

   function Create_Connection (Self   : Connection_Factory_Topic)
                               return MOMA.Connections.Connection'Class;

   function Create_Connection (Self      : Connection_Factory_Topic;
                               Username  : String;
                               Password  : String)
                               return MOMA.Connections.Connection'Class;

end MOMA.Connection_Factories.Topics;
