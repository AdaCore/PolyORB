----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object (corresponds to eg2_impl.cc in omniORB     ----
----                                                                    ----
----                server                                              ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba ; use Corba ;
with Corba.Object ;
with Corba.Orb ; use Corba.Orb ;
with Corba.Boa ; use Corba.Boa ;
with Text_IO ; use Text_Io ;
with Echo.Impl ;
with Cosnaming ;
use type Cosnaming.Name ;
with Corba_Initialreferences ;
with Cosnaming.Namingcontext ;

procedure server is
   -- Initialisation of The ORB
   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2") ;

   -- Initialisation of the BOA
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;

   -- IOR of the Naming service
   Ior : Corba.String := Corba.To_Corba_String
     ("IOR:019f00402800000049444c3a6f6d672e6f72672f436f734e616d696e672f4e616d6"
      & "96e67436f6e746578743a312e3000010000000000000028000000010100000e000000"
      & "3133372e3139342e31342e333000d1070c0000003703ee7d00cb167700000002") ;

   Root_context : Cosnaming.Namingcontext.Ref ;
   ObjName : Cosnaming.Name := Cosnaming.Name (Cosnaming.IDL_SEQUENCE_NameComponent.Null_Sequence) ;
   El : Cosnaming.NameComponent ;

   Myecho : Echo.Impl.Object ;
   Myecho_Ref : Echo.Ref ;

begin

   -- binding the object with the ORB
   Object_Is_Ready(Boa, Myecho) ;

   Put_Line("The echo object is ready");

   -- get the root_context of the name_service
   Corba.Orb.String_To_Object(Ior, Root_context) ;

   -- compute the name of the service
   El := (Id => Cosnaming.IString(Corba.To_Corba_String(Standard.String'("echo"))),
          Kind => Cosnaming.IString(Corba.To_Corba_String(Standard.String'("object")))) ;
   ObjName := ObjName & El ;

   -- getting a Ref object
   Myecho_Ref := Echo.To_Ref(Myecho) ;

   begin
      -- binding the Ref object to the naming service
      Cosnaming.Namingcontext.Bind (Root_Context,
                                    ObjName,
                                    Corba.Object.Ref (Myecho_Ref)) ;
   exception
      when COSNAMING.NAMINGCONTEXT.ALREADYBOUND =>
         Cosnaming.Namingcontext.Rebind (Root_Context,
                                         ObjName,
                                         Corba.Object.Ref (Myecho_Ref)) ;
   end ;

   Put_Line("The echo object suscribed to the naming service");

   -- launch the server
   Implementation_Is_Ready(Boa) ;

end ;



