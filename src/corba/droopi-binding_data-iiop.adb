--  Example binding data concrete implementation.

--  $Id$

with Droopi.Transport.Sockets;
with Droopi.Protocols.Giop;
with Droopi.Protocols;
with Droopi.Filters.Slicers;
--  The IIOP protocol is defined upon TCP/IP.

package body Droopi.Binding_Data.Iiop is

   use Droopi.Objects;

   procedure Initialize (P : in out Iiop_Profile_Type) is
   begin
      P.Object_Id := null;
   end Initialize;

   procedure Adjust (P : in out Iiop_Profile_Type) is
   begin
      if P.Object_Id /= null then
         P.Object_Id := new Object_Id'(P.Object_Id.all);
      end if;
   end Adjust;

   procedure Finalize (P : in out Iiop_Profile_Type) is
   begin
      Free (P.Object_Id);
   end Finalize;

   function Get_Object_Key
     (Profile : Iiop_Profile_Type)
     return Objects.Object_Id is
   begin
      return Profile.Object_Id.all;
   end Get_Object_Key;


   procedure Bind_Profile
     (Profile : Iiop_Profile_Type;
      TE      : out Transport.Transport_Endpoint_Access;
      Session : out Components.Component_Access)
   is
      use Droopi.Protocols.Giop;
      use Droopi.Sockets;
      use Droopi.Transport.Sockets;

      S : Socket_Type;
      Remote_Addr : Sock_Addr_Type := Profile.Address;
      P : aliased Giop_Protocol;
      ORB : constant ORB_Access := ORB_Access (S.Server);
      Slicer_Fact :  Factory_Access  := new Factory_Slicer;

   begin
      Create_Socket (S);
      Connect_Socket (S, Remote_Addr);
      TE := new Transport.Sockets.Socket_Endpoint;
      Create (Socket_Endpoint (TE.all), S);
      Create (P'Access, Filters.Filter_Access (Session));

      Register_Endpoint
        (ORB, TE, Slicer_Fact, Session.Role);

      -- Connect  to filter slicer
      Connect_Lower(Session, Component_Access(Upper(TE)));
      Connect(Upper(TE).Upper,  Component_Access(Session));

   end Bind_Profile;


   function Get_Profile_Tag
     (Profile : Iiop_Profile_Type)
     return Profile_Tag is
   begin
      return Tag_Internet_Iop;
   end Get_Profile_Tag;

   function Get_Profile_Preference
     (Profile : Iiop_Profile_Type)
     return Profile_Preference is
   begin
      return Preference_Default;
   end Get_Profile_Preference;

   procedure Create_Factory
     (PF : out IIOP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access) is
   begin
      PF.Address := Address_Of (Socket_Access_Point (TAP.all));
   end Create_Factory;

   function Create_Profile
     (PF  : access Iiop_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      Oid : Objects.Object_Id)
     return Profile_Access
   is
      use Droopi.Transport.Sockets;

      Result : constant Profile_Access
        := new Iiop_Profile_Type;

      TResult : Iiop_Profile_Type
        renames Iiop_Profile_Type (Result.all);
   begin
      TResult.Object_Id := new Object_Id'(Oid);
      TResult.Address   := Address_Of
        (Socket_Access_Point (TAP.all));
      return  Result;
   end Create_Profile;

   function Is_Local_Profile
     (PF : access IIOP_Profile_Factory;
      P : Profile_Access) return Boolean is
   begin
      return P.all in IIOP_Profile_Type
        and then IIOP_Profile_Type (P.all).Address = PF.Address;
   end Is_Local_Profile;

   --------------------------------
   -- Marshall_IIOP_Profile_Body --
   --------------------------------

   procedure Marshall_IIOP_Profile_Body
     (IOR     : access Buffer_Type;
      Profile : access Profile_Type'Class)
   is
      use representations.CDR;

      IIOP_Profile : IIOP_Profile_Type renames IIOP_Profile_Type (Profile.all);
      Profile_Body : aliased Buffer_Type;

   begin

      --  A TAG_INTERNET_IOP Profile Body is an encapsulation.

      Start_Encapsulation (Profile_Body'Access);

      --  Version
      Marshall (Profile_Body'Access, CORBA.Octet'(IIOP_Major_Version));
      Marshall (Profile_Body'Access, CORBA.Octet'(IIOP_Minor_Version));

      -- Marshalling of a Socket
      --Marshall (Profile_Body'Access, IIOP_Profile.);
      --Marshall (Profile_Body'Access, IIOP_Profile.Port);
      Marshall (Profile_Body'Access, IIOP_Profile.ObjKey);

      --  Marshall the Profile_Body into IOR.
      Marshall (IOR, Encapsulate (Profile_Body'Access));
      Release (Profile_Body);
   end Marshall_IIOP_Profile_Body;


   --------------------------------
   -- Marshall_IIOP_Profile_Body --
   --------------------------------

   procedure Unmarshall_IIOP_Profile_Body
     (Buffer   : access Buffer_Type;
      Profile  : out Profile_Ptr)
   is

   begin

    -- not yet implemnted

   end Unmarshall_IIOP_Profile_Body;



end Droopi.Binding_Data.Iiop;
