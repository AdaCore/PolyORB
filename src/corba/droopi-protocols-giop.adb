------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           B R O C A . G I O P                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams; Uses Ada.Streams;
with Ada.Unchecked_Deallocation;

with CORBA;
with CORBA.NvList;
with Droopi.Representations.CDR;

with Droopi.Exceptions;
with Sequences.Unbounded;
with Droopi.Opaque; use Droopi.Opaque;
with Droopi.Buffers;use Droopi.Buffers;
with Droopi.Binding_Data; use Droopi.Binding_Data;
with Droopi.Protocols; use Droopi.Protocols;
with Droopi.References;
with Droopi.Obj_Adapters.Simple;
with Droopi.Orb;


pragma Elaborate_All (Droopi.Log);

package body Droopi.Protocols.GIOP is


   package L is new Droopi.Log.Facility_Log ("droopi.protocols.giop");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   type Bits_8 is CORBA.Octet;

   -- added from broca.sequences

   Magic : constant Octet_Array :=
     (Character'Pos ('G'),
      Character'Pos ('I'),
      Character'Pos ('O'),
      Character'Pos ('P'));


   --  The offset of the byte_order boolean field in
   --  a GIOP message header.

   Prof_Factory : Filters.Factory  :=new Iiop_Profile_Factory;

   Response_Flags: constant array(range 0..3) of CORBA.Octet:=
                   (0,16#1#, 16#2#, 16#3#) ;

   Pend_Req : Pending_Request;

   Mess_Par :  Message_Param;

   Current_Request_Id : CORBA.Unsigned_Long :=1;

   GIOP_Error  : exception;

   MsgType_To_Octet :
     constant array (Msg_Type'Range) of CORBA.Octet
     := (Request          => 0,
         Reply            => 1,
         Cancel_Request   => 2,
         Locate_Request   => 3,
         Locate_Reply     => 4,
         Close_Connection => 5,
         Message_Error    => 6,
         Fragment         => 7);

   ReplyStatusType_To_Unsigned_Long :
     constant array (Reply_Status_Type'Range) of CORBA.Unsigned_Long
     := (No_Exception     => 0,
         User_Exception   => 1,
         System_Exception => 2,
         Location_Forward => 3,
         Location_Forward_Perm => 4,
         Needs_Addressing_Mode => 5);

   LocateStatusType_To_Unsigned_Long :
     constant array (Locate_Status_Type'Range) of CORBA.Unsigned_Long
     := (Unknown_Object => 0,
         Object_Here    => 1,
         Object_Forward => 2
         Object_Forward_Perm => 3,
         Loc_System_Exception=> 4,
         Loc_Needs_Addressing_Mode => 5);

   Octet_To_MsgType :
     constant array (CORBA.Octet range 0 .. 7) of Msg_Type
     := (0 => Request,
         1 => Reply,
         2 => Cancel_Request,
         3 => Locate_Request,
         4 => Locate_Reply,
         5 => Close_Connection,
         6 => Message_Error,
         7 => Fragment);

   Unsigned_Long_To_ReplyStatusType :
     constant array (CORBA.Unsigned_Long range 0 .. 3) of Reply_Status_Type
     := (0 => No_Exception,
         1 => User_Exception,
         2 => System_Exception,
         3 => Location_Forward,
         4 => Location_Forward_Perm,
         5 => Needs_Addressing_Mode);

   Unsigned_Long_To_LocateStatusType :
     constant array (CORBA.Unsigned_Long range 0 .. 2) of Locate_Status_Type
     := (0 => Unknown_Object,
         1 => Object_Here,
         2 => Object_Forward
         3 => Object_Forward_Perm,
         4 => Loc_System_Exception,
         5 => Loc_Needs_Addressing_Mode);


  --------------------------
  --  Some utilities functions
  ----------------------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in MsgType) is
   begin
      Marshall(Buffer, CORBA.Octet'(MsgType'Pos(Value)));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in ReplyStatusType) is
   begin
      Marshall(Buffer, CORBA.Octet'(ReplyStatusType'Pos(Value));
   end Marshall;


   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in LocateStatusType) is
   begin
      Marshall (Buffer, CORBA.Octet'(LocateStatusType'Pos(Value));
   end Marshall;


   function Unmarshall
     (Buffer : access Buffer_Type)
     return MsgType is
   begin
      return MsgType'Val(Unmarshall(Buffer));
   end Unmarshall;


   function Unmarshall
     (Buffer : access Buffer_Type)
     return ReplyStatusType is
   begin
      return ReplyStatusType'Val(Unmarshall (Buffer));
   end Unmarshall;


   function Unmarshall
     (Buffer : access Buffer_Type)
     return LocateStatusType is
   begin
      return LocateStatusType'Val(Unmarshall (Buffer));
   end Unmarshall;



   ----------------------------------------------------
   -- Some Marshalling procedures of GIOP 1.0 and 1.1
   ---------------------------------------------------


   ----------------------------------
   -- Marshalling of Reply messages
   ----------------------------------


   -------------------------------------
   --- Cancel Request Message Marshalling
   -------------------------------------


  procedure Cancel_Request_Marshall
     (Buffer           : access Buffer_Type;
      Request_Id       : in Corba.Unsigned_Long)

  is
      use Corba.CDR;
  begin

      --  Reserve space for message header
      Set_Initial_Position
       (Buffer, Message_Header_Size );

      -- Request id
      Marshall(Buffer, Request_Id);

  end Cancel_Request_Marshall;

 ------------------------------------------------
 ---  Locate Request Message Marshall
 -----------------------------------------------


   procedure Locate_Request_Marshall
     (Buffer           : access Buffer_Type;
      Request_Id       : in CORBA.Unsigned_Long;
      Object_Key       : in Objects.Object_Id)

   is
      use representations.CDR;
   begin

      --  Reserve space for message header
      Set_Initial_Position
       (Buffer, Message_Header_Size);

      -- Request id
      Marshall (Buffer, Request_Id);

      -- Object Key
      Marshall(Buffer, Object_key);

   end Locate_Request_Marshall;

  -------------------------------------
  --- Locate_Reply_Marshall ----------
  ----------------------------------

  procedure Locate_Reply_Marshall
   ( Buffer         : access Buffer_Type;
     Request_Id     : in Corba.Unsigned_Long;
     Locate_Status  : in Locate_Reply_Status)
  is
      use representations.CDR;
  begin

      --  Reserve space for message header
      Set_Initial_Position
        (Buffer, Message_Header_Size);

       -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, Locate_Status);
  end Locate_Reply_Marshall;



 ----------------------------
   -- Unmarshall_GIOP_Header --
 ----------------------------

   procedure GIOP_Header_Unmarshall
     (Ses                   : access GIOP_Session;
      Message_Type          : out Msg_Type;
      Message_Size          : out CORBA.Unsigned_Long;
      Fragment_Next         : out CORBA.Boolean;
      Success               : out Boolean)
   is

      use representations.CDR;
      --Buffer_In : Buffer_Type renames S.Buffer_In;
      Message_Magic : Element_Stream_Array(Magic'Range);
      Message_Major_Version : CORBA.Octet;
      Message_Minor_Version : CORBA.Octet;
      Flags :  CORBA.Octet;
   begin

      Success := False;

      --  Magic
      for I in Message_Magic'Range loop
         Message_Magic (I) := Opaque.Octet
           (CORBA.Octet'(Unmarshall (Ses.Buffer_In)));
      end loop;

      if Message_Magic /= Magic then
         pragma Debug (O ("Unmarshall_GIOP_Header: Bad magic!"));
         return;
      end if;

      --  test if the GIOP version of the Message received is supported
      Message_Major_Version := Unmarshall (Ses.Buffer_In);
      Message_Minor_Version := Unmarshall (Ses.Buffer_In);

      if not (Message_Major_Version =  Ses.Major_Version)
        or else (Ses.Minor_Version < Message_Minor_Version) then
         pragma Debug(0 ("Unmarshall_GIOP_Header: GIOP version not supported"));
         return;
      end if;

      if Message_Minor_Version=0 then

         --  Byte order
         if Unmarshall (Ses.Buffer_In) = 0  then
             Set_Endianness(Ses.Buffer_In, Little_Endian);
         else
             Set_Endianness(Ses.Buffer_In, Big_Endian);
         end if;


      else
         -- Flags
         Flags := Unmarshall(Buffer_In);

         if (Flags and 2**Endianess_Bit = True) then
             Set_Endianness(Ses.Buffer_In, Little_Endian);
         else
             Set_Endianness(Ses.Buffer_In, Big_Endian);
         end if;

         if (Flags and 2**Fragment_Bit = true) then
             Fragment_Next := true;
         else
             Fragment_Next := False;
         end if;

      end if;

      --  Message type
      Message_Type := Unmarshall (Ses.Buffer_In);

      --  Message size
      if Message_Endianness = Big_Endian

         Message_Size := Unmarshall(Ses.Buffer_In);
      else

      end if;

      -- everything allright
      Ses.Major_Version := Message_Major_Version;
      Ses.Minor_Version := Message_Minor_Version;

      Success := True;

   end Unmarshall_GIOP_Header;


   ----------------------------------------
   --  Unmarshall Locate Request
   ----------------------------------


   procedure Request_Message_Unmarshall
     (Buffer        : access Buffer_Type;
      Request_Id    : out Corba.Unsigned_Long;
      Object_Key    : out Objects.Object_Id)
   is
      use representations.CDR;
   begin
      --  Request id
      Request_Id := Unmarshall(Buffer);

      -- Reply Status
      Object_Key := Unmarshall(Buffer);

   end Reply_Message_Unmarshall;



    ----------------------------------------
   --  Unmarshall Locate Reply
   ----------------------------------


   procedure Locate_Reply_Unmarshall
     (Buffer        : access Buffer_Type;
      Request_Id    : out Corba.Unsigned_Long;
      Locate_Status : out Locate_Status_Type)
   is
      use representations.CDR;
   begin
      --  Request id
      Request_Id := Unmarshall(Buffer);

      -- Reply Status
      Reply_Status := Unmarshall(Buffer);

   end Reply_Message_Unmarshall;




   ---------------------------------------------------------
   -- Switch for  GIOP version Messages --------------------
   ---------------------------------------------------------

   --------------------
   --  Request
   -------------------

  procedure Request_Message
    (Ses               : access GIOP_Session;
     Target_Profile    : in Binding_Data.IIOP_Profile_Type;
     Response_Expected : in Boolean;
     Message_Size      : in Corba.Unsigned_Long;
     Fragment_Next     : out boolean)
  is
     Header_Buffer : aliased Buffer_Type;
     Body_Buffer   : aliased Buffer_Type;
     Sync          : Sync_Scope;
  begin

    Fragment_Next= False;

   case Ses.Minor_Version is
    when  0=>

         GIOP.GIOP_1_0.Request_Message_Marshall
           (Body_Buffer'Access, Pend_Req.Request_Id,
            Ses.Target_Profile, Response_Expected),
            Pend_Req.Req.Operation);

         GIOP.GIOP_1_0.GIOP_Marshall_Header(Header_Buffer'Access,
                       GIOP.Request,  Message_Size+ Length(Body_Buffer'Access));


    when 1=>
         GIOP.GIOP_1_1.Request_Message_Marshall
           (Body_Buffer'Access, Pend_Req.Request_Id,
            Ses.Target_Profile, Response_Expected,
            Pend_Req.Req.Operation);

         if Message_Size+Length(Body_Buffer'Access)> Maximum_Message_Size then
                Fragment_Next =True;
         end if;

         GIOP.GIOP_1_1.GIOP_Marshall_Header(Header_Buffer'Access,
                       GIOP.Request, Message_Size+Length(Body_Buffer'Access),
                       Fragment_Next);

    when 2=>
        if Response_Next then
           Sync = WITH_TARGET
        else
           Sync = NONE
        end if;
        GIOP.GIOP_1_2.Request_Message_Marshall
           (Body_Buffer'Access, Pend_Req.Request_Id,
            Ses.Target_Profile, Sync,
            Pend_Req.Req.Operation);

        if Message_Size+Length(Body_Buffer'Access)> Maximum_Message_Size then
                Fragment_Next =True;
         end if;

        GIOP.GIOP_1_2.GIOP_Marshall_Header(Header_Buffer'Access,
              GIOP.Request,  Message_Size+Length(Body_Buffer'Access),
              Fragment_Next);

    end case;

  Prepend(Header_Buffer, Body_Buffer'Access);
  Ses.Buffer_Out=Body_Buffer;

 end Request_Message;

 ----------------------------------------
 -- No Exception Reply
 ---------------------------------------

 procedure No_Exception_Reply
    (Ses           : access GIOP_Session;
     Request_Id    : in CORBA.Unsigned_Long;
     Message_Size  : in CORBA.Unsignd_Long;
     Fragment_Next : out boolean)

 is
   Header_Buffer : aliased Buffer_Type;
   Body_Buffer   : aliased Buffer_Type;
 begin

  Fragment_Next= False;

  case Ses.Minor_Version is
   when  0=>

         GIOP.GIOP_1_0.No_Exception_Marshall(Body_Buffer'Access, Request_Id);

         GIOP.GIOP_1_0.GIOP_Marshall_Header(Header_Buffer'Access,
                       GIOP.Reply, Message_Size + Length(Body_Buffer'Access);

   when 1=>

         GIOP.GIOP_1_1.No_Exception_Marshall(Body_Buffer'Access, Request_Id);

         if Message_Size+ Length(Body_Buffer'Access) > Maximum_Message_Size then
                Fragment_Next =True;
         end if;

         GIOP.GIOP_1_1.GIOP_Marshall_Header(Header_Buffer'Access,
                       GIOP.Reply, Message_Size+ Length(Body_Buffer'Access), Fragment_Next);

   when 2=>

         GIOP.GIOP_1_2.No_Exception_Marshall(Body_Buffer'Access, Request_Id);

         if Message_Size + Length(Body_Buffer'Access) > Maximum_Message_Size then
                Fragment_Next =True;
         end if;

         GIOP.GIOP_1_2.GIOP_Marshall_Header(Header_Buffer'Access,
                       GIOP.Reply, Message_Size+Length(Body_Buffer'Access), Fragment_Next);
  end case;
  Prepend(Header_Buffer, Body_Buffer'Access);
  Ses.Buffer_Out=Body_Buffer;
 end;


 ----------------------------------------
 -- Exception
 ---------------------------------------

 procedure Exception_Reply
    (Ses             : access GIOP_Session;
     Exception_Type  : in Reply_Status_Type range User_Exception..System_Exception;
     Message_Size    : in CORBA.Unsigned_Long;
     Occurence       : in CORBA.Exception_Occurrence)

 is
    Header_Buffer : aliased Buffer_Type;
    Body_Buffer   : aliased Buffer_Type;
 begin

  case Ses.Minor_Version is
   when  0=>

         GIOP.Exception_Marshall
           (Body_Buffer'Access, Pend_Req.Request_Id,
                Exception_Type , Occurence);

         GIOP.GIOP_1_0.GIOP_Marshall_Header(Header_Buffer'Access,
                       GIOP.Reply,  Message_Size+ Length(Body_Buffer'Access);

   when 1=>

        GIOP.Exception_Marshall(Body_Buffer'Access,
                       Pend_Req.Request_Id, Exception_Type, Occurence );

        if Message_Size + Length(Body_Buffer'Access)> Maximum_Message_Size then
                Fragment_Next =True;
        end if;

        GIOP.GIOP_1_1.GIOP_Header_Marshall(Header_Buffer'Access,
                       GIOP.Reply, Message_Size, Fragment_Next);

   when 2=>

        GIOP.GIOP_1_2.Exception_Marshall(Body_Buffer'Access,
                       Pend_Req.Request_Id, Exception_Type, Occurence);

        if Message_Size + Length(Body_Buffer'Access) > Maximum_Message_Size then
                Fragment_Next =True;
        end if;

        GIOP.GIOP_1_2.GIOP_Header_Marshall(Header_Buffer'Access,
                       GIOP.Reply, Message_Size + Length(Body_Buffer'Access), Fragment_Next);

   when others =>
        raise GIOP_ERROR;
  end case;

  Prepend(Header_Buffer, Body_Buffer'Access);
  Ses.Buffer_Out=Body_Buffer;

 end  Exception_Reply;

  -------------------------------------------------------
  --  Location Forward
  --------------------------------------------------------

  procedure Location_Forward_Reply
    (Ses             : access GIOP_Session;
     Exception_Type  : in Reply_Status_Type range Location_Forward .. Location_Forward_Perm;
     Forward_Ref     : in Droopi.References.Ref;
     Message_Size    : in CORBA.Unsigned_Long;
     Fragment_Next   : out Boolean)

  is
    Header_Buffer : aliased Buffer_Type;
    Body_Buffer   : aliased Buffer_Type;
  begin

   Fragment_Next=False;

   case Ses.Minor_Version is
    when  0=>

         GIOP.Location_Message_Marshall
             (Body_Buffer'Access , Pend_Req.Request_Id,
              Forward_Ref);

         GIOP.GIOP_1_0.GIOP_Header_Marshall(Header_Buffer'Access,
                       GIOP.Reply,  Message_Size + Length(Body_Buffer'Access))

    when 1=>

         GIOP.Location_Message_Marshall
             (Body_Buffer'Access , Pend_Req.Request_Id,
              Forward_Ref);


         if Message_Size + Length(Body_Buffer'Access) > Maximum_Message_Size then
                Fragment_Next =True;
         end if;

         GIOP.GIOP_1_1.GIOP_Header_Marshall(Head_Buffer'Access ,
                       GIOP.Reply , Message_Size +Length(Body_Buffer'Access), Fragment_Next);

    when 2=>

        GIOP.GIOP_1_2.Location_Message_Marshall(Body_Buffer'Access,
                       Pend_Req.Request_Id, Forward_Ref);


        if Message_Size+ Length(Body_Buffer'Access) >Maximum_Message_Size then
                Fragment_Next =True;
        end if;

        GIOP.GIOP_1_2.GIOP_Header_Marshall(Header_Buffer'Access,
                       GIOP.Reply, Message_Size+Length(Body_Buffer'Access), Fragment_Next);

    when others =>
        raise GIOP_ERROR;

  end case;

  Prepend(Header_Buffer, Body_Buffer'Access);
  Ses.Buffer_Out=Body_Buffer;

  end Location_Forward_Reply;


  -------------------------------------------------------
  --  Need_Addressing_Mode_Message
  --------------------------------------------------------

 procedure Need_Addressing_Mode_Message
    (Ses             : access GIOP_Session;
     Message_Size    : in CORBA.Unsigned_Long;
     Address_Type    : in Addressing_Disposition)

  is
    Header_Buffer : aliased Buffer_Type;
    Body_Buffer   : aliased Buffer_Type;
  begin

   Fragment=False;


   if Ses.Minor_Version /=  2 then
      raise GIOP_Error;
   end if;


   GIOP.GIOP_1_2.Needs_Addressing_Mode_Marshall
         (Body_Buffer, Pend_Req.Request_Id, Address_Type);


   GIOP.GIOP_1_2.GIOP_Header_Marshall(Header_Buffer'Access ,
                       GIOP.Reply, Message_size+Length(Body_Buffer'Access) , False);
   Prepend(Header_Buffer, Body_Buffer'Access);
   Ses.Buffer_Out=Body_Buffer;

  end Need_Addressing_Mode_Message;



  -------------------------------------------------------
  --  Cancel Request
  --------------------------------------------------------

  procedure Cancel_Request_Message
    (Ses             : access GIOP_Session;
     Message_Size    : in CORBA.Unsigned_Long)

  is
    Header_Buffer : aliased Buffer_Type;
    Body_Buffer   : aliased Buffer_Type;
  begin

   case Ses.Minor_Version is
    when  0=>

         GIOP.Cancel_Request_Marshall
              (Body_Buffer'Access, Pend_Req.Request_Id);
         GIOP.GIOP_1_0.GIOP_Header_Marshall(Header_Buffer,
                       GIOP.Cancel_Request,Message_size+Length(Body_Buffer'Access));

    when 1=>

         GIOP.Cancel_Request_Marshall
              (Body_Buffer'Access, Pend_Req.Request_Id);
         GIOP.GIOP_1_1.GIOP_Header_Marshall(Header_Buffer,
                       GIOP.Cancel_Request, Message_Size+Length(Body_Buffer'Access), False);

    when 2=>

         GIOP.Cancel_Request_Marshall
              (Body_Buffer'Access, Pend_Req.Request_Id);
         GIOP.GIOP_1_2.GIOP_Header_Marshall(Header_Buffer,
                       GIOP.Cancel_Request, Message_Size+Length(Body_Buffer'Access), False);


    when others =>
        raise GIOP_ERROR;

   end case;

   Prepend(Header_Buffer, Body_Buffer'Access);
   Ses.Buffer_Out=Body_Buffer;

  end Cancel_Request_Message;


  ----------------------------------------
  --  Locate Request
  ----------------------------------------

  procedure Locate_Request_Message
    (Ses             : access GIOP_Session;
     Object_key      : in Objects.Objects_Id;
     Fragment_Next   : out Boolean)
  is
    Header_Buffer : aliased Buffer_Type;
    Body_Buffer   : aliased Buffer_Type;
  begin

   Fragment_Next = False;

   case Ses.Minor_Version is
    when  0=>

         GIOP.Locate_Request_Marshall
            (Body_Buffer'Access, Pend_Req.Request_Id,
             Object_Key);
         GIOP.GIOP_1_0.GIOP_Header_Marshall(Header_Buffer'Access,
                       GIOP.Locate_Request, Length(Body_Buffer'Access));

    when 1=>
         GIOP.Locate_Request_Marshall
            (Body_Buffer'Access, Pend_Req.Request_Id,
             Object_Key);
         GIOP.GIOP_1_1.GIOP_Header_Marshall(Header_Buffer'Access,
                       GIOP.Locate_Request, Length(Body_Buffer'Access), false);

    when 2=>
         GIOP.Locate_Request_Marshall
            (Body_Buffer'Access, Pend_Req.Request_Id, 0,
             Object_Key);

         if Message_Size + Length(Body_Buffer'Access) > Maximum_Message_Size then
                Fragment_Next =True;
         end if;

         GIOP.GIOP_1_2.GIOP_Header_Marshall(Header_Buffer'Access,
                       GIOP.Locate_Request, Length(Body_Buffer'Access), false);

    when others =>
        raise GIOP_ERROR;
   end case;

   Prepend(Header_Buffer, Body_Buffer'Access);
   Ses.Buffer_Out=Body_Buffer;

  end Location_Request_Message;



  ----------------------------------------
  --  Locate Reply
  ----------------------------------------

  procedure Locate_Reply_Message
    (Ses             : access GIOP_Session;
     Locate_Status   : in Locate_Status_Type;
     Message_Size    : in Corba.Unsigned_Long)

  is
    Header_Buffer : aliased Buffer_Type;
    Body_Buffer   : aliased Buffer_Type;
  begin


    if (Ses.Minor_Version=0 or Ses.Minor_Version=1) and
       (Locate_Status= Object_Forward_Perm or Locate_Status= Sr_Loc_System_Exception
        or Locate_Status= Sr_Loc_Needs_Addressing_Mode) then
      raise GIOP_Error;
    end if;

    GIOP.Locate_Reply_Marshall
         (Body_Buffer, Pend_Req, Locate_Status);

    case Ses.Minor_Version is
     when  0=>

         GIOP.GIOP_1_0.GIOP_Header_Marshall(Header_Buffer'Access,
                       GIOP.Locate_Reply, Message_size+Length(Body_Buffer'Access));

     when 1=>

         GIOP.GIOP_1_1.GIOP_Header_Marshall(Header_Buffer'Access,
                       GIOP.Locate_Reply, Message_Size+Length(Body_Buffer'Access), false);

     when 2=>

         GIOP.GIOP_1_2.GIOP_Header_Marshall(Header_Buffer'Access,
                       GIOP.Locate_Reply, Message_Size+Length(Body_Buffer'Access), false);

     when others =>
        raise GIOP_ERROR;
    end case;

    Prepend(Header_Buffer, Body_Buffer'Access);
    Ses.Buffer_Out=Body_Buffer;

  end Location_Request_Message;


   -----------------------------------------
   -- Selecting the IIOP profile from an IOR
   -----------------------------------------

   procedure Select_Profile
     (Buffer  : access Buffer_Type;
      Profile : out Binding_Data.Profile_Type)
   is
     use Droopi.Representations.CDR;
     New_Ref    : Droopi.References.Ior.Ior_Type
     Prof_Array : Droopi.References.Profile_Array;
     Prof_Temp  : Binding_Data.Profile_Type;
   begin
      pragma Debug(O ("Reply Message : Received Location_Forward"));
      Unmarshall(Buffer, New_Ref);
      Prof_Array := Profiles_Of(Ref'(New_Ref));
      for I in Prof_Array'Range loop
         if Get_Profile_Tag(Prof_Array(I)) = Tag_Internet_IOP then
               Profile = (Prof_Array(I))'Access;
              exit;
         end if;
      end loop;
      Profile := Prof_Temp;
   end Select_Profile;


  ------------------------------------
  --   pending requests
  ------------------------------------

    procedure Store_Request
     (Req  : access Request)
    is

    begin

      Pend_Req.Req := Req;
      Pend_Req.Request_Id := Current_Request_Id;
      Current_Request_Id := Current_Request_Id +1;
    end Store_Request;



  -----------------------------------------
  ---   Receiving a  Request Message
  -----------------------------------------

  procedure Request_Received
    (Ses               : access GIOP_Session)

   is

      use Obj_Adapters.Simple;

      Request_Id        :  Corba.Unisgned_Long;
      Response_Expected :  Boolean;
      Object_Key        :  String;
      Oid               :  Objects.Object_Id;
      Operation         :  Requests.Operation_Id;
      Principal         :  Stream_Element_Array;


      Req    : Request_Access := new Request;
      Args   : CORBA.NVList.Ref;
      Obj    : CORBA.NVList.Object;
      Result : CORBA.NamedValue;
      Target_Profile : Binding_Data.Profile_Access
           := new Iiop_Profile_Type;
      Target : References.Ref;
      Target_Swsitch : TargetAddress;
      ORB : constant ORB_Access := ORB_Access (Ses.Server);
      Transp_AP : Transport_Access_Point := new Socket_Access_Point;


   Begin

    case Ses.Minor_Version is
      when  0=>

          GIOP.GIOP_1_0.Request_Message_Unmarshall(Ses.Buffer_In,
               Request_Id, Response_Expected, Object_Key ,Operation,
               Principal);

      when 1=>

          GIOP.GIOP_1_1.Request_Message_Unmarshall(Ses.Buffer_In,
               Request_Id, Response_Expected, Object_Key ,Operation,
               Principal);

      when 2=>

          GIOP.GIOP_1_2.Request_Message_Unmarshall(Ses.Buffer_In,
               Request_Id, Response_Expected, Target_Ref, Operation);
      when others =>
          raise GIOP_Error;
          -- shoulde never happen
     end case;

     Oid := To_Oid(Object_Key);

     Args   := Obj_Adapters.Get_Empty_Arg_List
              (Object_Adapter (ORB).all, Oid, Operation);

     Obj :=  Object_Of(Args);

    for I in 1 .. Get_Count(Args) loop
      Representations.Cdr.Unmarshall(Ses.Buffer_In, Element_Of(Obj.List, I));
    end loop;

    -- unmarshalling of arguments not yet implemented
    Result := (Name     => To_CORBA_String ("Result"),
               Argument => Obj_Adapters.Get_Empty_Result
               (Object_Adapter (ORB).all, Object_key, Operation);
               Arg_Modes => 0);

    if Ses.Minor_Version /= 2 then

      Transp_AP.Socket_Type := Lower(Lower(Ses)).Socket_Type;

      Target_Profile := Create_Profile
                           (Prof_Factory, Transp_AP, Object_Key);

      Create_Reference ((1 => Target_Profile), Target);
    else
      if  Target_Ref.Addess_Type = 1 then
          Create_Reference((1 => Target_Ref.Profile), Target);
      else
          Target := Target_Ref.Ref;
      end if;
    end if;

    Create_Request
              (Target    => Target,
               Operation => Method,
               Arg_List  => Args,
               Result    => Result,
               Req       => Req);

    Emit_No_Reply
      (Component_Access (ORB),
       Queue_Request'(Request => Req,
                     Requestor => Component_Access (Ses)));

    Store_Req(Req);

  end Request_Received;


  -----------------------------------------
  ---   Receiving a  Reply Message
  -----------------------------------------

  procedure Reply_Received
    (Ses               : access GIOP_Session)

  is
      Reply_Status  : Reply_Status_Type;
      Result : CORBA.NamedValue;

      Req    : Request_Access := new Request;
      Args   : CORBA.NVList.Ref;
      Result : CORBA.NamedValue;
      Target_Profile : Binding_Data.Profile_Access
           := new Iiop_Profile_Type;
      Target : References.Ref;
      Target_Switch : TargetAddress;
      ORB : constant ORB_Access := ORB_Access (Ses.Server);

      Transp_AP : Transport_Access_Point := new Socket_Access_Point ;

  Begin

    case Ses.Minor_Version is
      when  0=>
          GIOP.GIOP_1_0. Reply_Message_Unmarshall(Ses.Buffer_In,
               Request_Id,Reply_Status);
          if Reply_Status = Location_Forward_Perm or
              Reply_Status = Needs_Addressing_Mode then
              raise GIOP_Error;
          end if;


      when 1=>
          GIOP.GIOP_1_1. Reply_Message_Unmarshall(Ses.Buffer_In,
               Request_Id,Reply_Status);
          if Reply_Status = Location_Forward_Perm or
              Reply_Status = Needs_Addressing_Mode then
              raise GIOP_Error;
          end if;

      when 2=>
          GIOP.GIOP_1_2.Reply_Message_Unmarshall(Ses.Buffer_In,
               Request_Id,Reply_Status);
    end case;

    if Request_Id /= Pend_Req.Request_Id then
      raise GIOP_Error;
    end if;

    case Reply_Status is

      when No_Exception =>

          Pend_Req.Req.Result :=
               (Name     => To_CORBA_String ("Result"),
                Argument => Obj_Adapters.Get_Empty_Result
                (Object_Adapter (ORB).all, Get_Object_Id(Pend_Req.Target_Profile),
                Pend_Req.Req.Operation'Access, Arg_Modes => ARG_OUT);

          Representations.Cdr.Unmarshall(Ses.Buffer_In, Pend_Req.Req.Result);
          Emit_No_Reply
              (Component_Access (ORB),
               Queue_Request'(Request   => Pend_Req.Req,
                              Requestor => Component_Access (S)));


      when User_Exception =>

      when System_Exception =>
              CORBA.Exceptions.Unmarshall_And_Raise
                 (Buffer_In'Access);

      when Location_Forward | Location_Forward_Perm =>
           declare
              Sock : Socket_Type;
              TE_Socket : Socket_EndPoint := Lower(Lower(Ses));
           begin
              Select_Profile(Ses.Buffer_In, Pend_Req.Profile);
              Create_Socket (Sock);
              Connect_Socket(Sock, Pend_Req.Profile.Address);
              Create(Socket_Endpoint (TE_Socket.all), Sock);
              Invoque_Request(Ses, Pend_Req.Req);
           end;

      when Need_Addressing_Mode =>


    end case;

  end Reply_Received;


 ---------------------------------------------
 ---   receiving a locate request
 ----------------------------------------------

 procedure Locate_Request_Receive
    (Ses : access GIOP_Session)
 is

 begin

   case Ses.Minor_Version is
      when  0=>
          GIOP.GIOP_1_0. Locate_Request_Unmarshall(Ses.Buffer_In,
               Request_Id,Reply_Status);
          if Reply_Status = Location_Forward_Perm or
              Reply_Status = Needs_Addressing_Mode then
              raise GIOP_Error;
          end if;


      when 1=>
          GIOP.GIOP_1_1. Reply_Message_Unmarshall(Ses.Buffer_In,
               Request_Id,Reply_Status);
          if Reply_Status = Location_Forward_Perm or
              Reply_Status = Needs_Addressing_Mode then
              raise GIOP_Error;
          end if;

      when 2=>
          GIOP.GIOP_1_2.Reply_Message_Unmarshall(Ses.Buffer_In,
               Request_Id,Reply_Status);
    end case;

 end Locate_Request_Receive






 -----------------------------------------
 -- Selecting the IIOP profile from an IOR
 -----------------------------------------

 procedure Select_Profile
  (Buffer  : access Buffer_Type;
   Profile : out Binding_Data.Profile_Type)
 is
     use Droopi.Representations.CDR;
     New_Ref    : Droopi.References.Ior.Ior_Type
     Prof_Array : Droopi.References.Profile_Array;
     Prof_Temp  : Binding_Data.Profile_Type;
 begin
     pragma Debug(O ("Reply Message : Received Location_Forward"));
     Unmarshall(Buffer, New_Ref);
     Prof_Array := Profiles_Of(Ref'(New_Ref));
     for I in Prof_Array'Range loop
        if Get_Profile_Tag(Prof_Array(I)) = Tag_Internet_IOP then
              Profile = (Prof_Array(I))'Access;
              exit;
        end if;
     end loop;
     Profile := Prof_Temp;
 end Select_Profile;


  ------------------------------------
  --   pending requests
  ------------------------------------

  procedure Store_Request
     (Req  : access Request)
  is

  begin

   Pend_Req.Req := Req;
   Pend_Req.Request_Id := Current_Request_Id;
   Current_Request_Id := Current_Request_Id +1;

  end Store_Request;


   ------------------------------------
   --     exported functions
   ------------------------------------

   -----------------------------------------
   ---  Create the Session ---------------
   ---------------------------------------

   procedure Create
     (Proto   : access GIOP_Protocol;
      Session : out GIOP_Session)
   is

   begin

      Session := new GIOP_Session;

      GIOP_Session (Session.all).Buffer := new Buffers.Buffer_Type;
      GIOP_Session (Session.all).Out_Buffer := new Buffers.Buffer_Type;

   end Create;

  ------------------------------------
  --  Connect ------------------------
  ------------------------------------

   procedure Connect
     (S : access GIOP_Session)
   is
   begin
      -- connection not yet implemented
      -- S.Profile:=P;
   end Connect;


  -----------------------------------
  --   Invoque Request       --------
  -----------------------------------


   procedure Invoque_Request
     (Ses : access GIOP_Session;
      R   : access Requests.Request)
   is
     use Buffers;
     use Representations.Cdr;
     Buf1           : aliased Buffer_Type;
     Sli            : Filters.Slicers.Slicer_Filter;
     TE_Socket      : Socket_EndPoint;
     Fragment_Next  : Boolean;
     Arg_Count      : Corba.Long;
     Arg            : Corba.NamedValue;
     Data_Send      : Root_Data_Unit := new Data_Out;
     Message_Size   : Corba.Unsigned_Long;
   begin

    if Ses.Role  = Server then
      raise GIOP_Error;
    end if;

    Sli := Lower(Ses);
    TE_Socket := Lower(Sli);

    Buffer.Release_Contents(Buffer_Out);
    Store_Request(R);

    -- fragmentation not yet implemented
    -- Message_Size:= Length(Buf1);

    -- if Message_Size > Maximum_Body_Size then
    --    Buf2 :=
    -- end if;


    if (Ses.Object_Found = False) then
         if  Ses.Nb_Tries <= Max_Nb_Tries) then
           Locate_Request_Message(Ses, Ses.Object_Id, Fragment_Next);
           Ses.Nb_Tries = Ses.Nb_Tries + 1;
         else
           pragma Debug (O ("Number of tries exceeded"));
           return;
         end if;
    else
       --  Marshall the request's Body
       Arg_Count := Get_Count(R.Args);
       for I in 0 .. Arg_Count
           Arg = Element_Of(R.Args);
           Marshall(Buf1, Arg);
       end for;
       Message_Size := Length(Buf1);
       Request_Message(Ses, True, Fragment_Next);
       Prepend(Buf2, Ses.Buffer_Out);
       Ses.Object_Found := True;
       Ses.Nb_Tries := 0;
    end if;


    -- Sending the message
    Sli.In_Buf := Ses.Buffer_Out;
    Data_Send.Out_Buf := Sli.In_Buf;
    Handle_Message(Sli, Data_Send);

    -- Expecting data
    Expect_Data(Ses, Ses.Buffer_In, Header_Message_Size);

   end Invoque_Request;


   -------------------------------------
   --  Send Reply
   --------------------------------------

   procedure Send_Reply (Ses : access Echo_Session; R : Request)
   is
      use Buffers;
      use Representations.Cdr;
      Buf1      : aliased Buffer_Type;
      Data_Send : Root_Data_Unit := new Data_Out;

   begin
    if Ses.Role = Client then
     raise GIOP_Error;
    end if;

    Sli := Lower(Ses);
    TE_Socket := Lower(Sli);

    Buffer.Release_Contents(Buffer_Out);

    No_Exception_Reply(Ses, Pend_Req.Request_Id);
    --  Marshall the reply's Body
    Marshall(Buf1, Request.Result);

    Prepend(Buf1,Buffer_Out);

    -- Sending the message
    Sli.In_Buf := Ses.Buffer_Out;
    Data_Send.Out_Buf := Sli.In_Buf;
    Handle_Message(Sli, Data_Send);

    -- Expecting data
    Expect_Data(Ses, Ses.Buffer_In, Messahe_Header_Size);

   end Send_Reply;


   ----------------------------------
   --  Handle Connect Indication ----
   ----------------------------------

   procedure Handle_Connect_Indication (S : access Echo_Session)
   is
       Filt_Slicer : constant Filters.Filter := Lower(S)  ;
   begin
      pragma Debug (O ("Received new connection to echo service..."));

      Expect_Data(S, S.Buffer_In, Header_Message_Size);
      Emit_No_Reply
        (Port   => Lower(Filt_Slicer),
         Msg    => Data_Expected'(In_Buf => In_Buf, Max => Max_Data_Received));

   end Handle_Connect_Indication;


   procedure Handle_Connect_Confirmation (S : access Echo_Session) is
   begin
      null;
      --  No setup is necessary for newly-created client connections.
   end Handle_Connect_Confirmation;


   procedure Handle_Data_Indication (S : access GIOP_Session)
   is
      use CORBA;
      use CORBA.NVList;
      use Binding_Data.Iiop;
      use Objects;
      use References;

   begin
      pragma Debug (O ("Received data on socket service..."));
      pragma Debug (Buffers.Show (S.Buffer.all));

      declare
         Sli       : Filters.Slicers.Slicer_Filter;
        -- TE_Socket : Socket_EndPoint;
         Req    : Request_Access := null;
         Mess_Type : Msg_Type;

         --Args   : CORBA.NVList.Ref;
         --Result : CORBA.NamedValue;

         --Target_Profile : Binding_Data.Profile_Access
         --  := new Iiop_Profile_Type;
         --Target : References.Ref;

         --ORB : constant ORB_Access := ORB_Access (S.Server);

      begin
         pragma Debug (O ("Received request " & Method
                             & " on object " & Image (Oid)
                             & " with args " & Arg_String));

         if S.Expect_Header then
            Unmarshall_GIOP_Header(S, Mess_Type, Mess_Size,
                   Fragment_Next, Success);
            if not Success then
               raise GIOP_Error;
            end if;
            S.Mess_Type_Received  := Mess_Type;
            S.Expect_Header := False;
            Expect_Data(S, S.Buffer_In, Mess_Size);
            return;
         end if;

            --  if Fragment_Next then
            --   Expect_Data();
            --   return;
            --  end if

         case S.Mess_Type_Received  is
           when Request =>
              if Ses.Role = Server then
                 Request_Received(S);
              else
                 raise GIOP_Error;
              end if;
           when Reply =>
              if Ses.Role = Client then
                  Reply_Received(S);
              else
                 raise GIOP_Error;
              end if;

           when Cancel_Request =>
              if Ses.Role=Client then

              else
                  raise GIOP_Error;
              end if;

           when Locate_Request =>
              if Ses.Role=Server then

              else
                  raise GIOP_Error;
              end if;

           when Locate_Reply =>
              if Ses.Role = Client  then
                declare
                   Req_Id        : CORBA.Unsigned_Long;
                   Locate_Status : Locate_Status_Type;
                begin
                    Locate_Reply_Unmarshall(Ses.Buffer_In, Req_Id,
                           Locate_Status);
                    case Locate_Status is
                       when Object_Here =>
                          Ses.Object_Found := True;
                          Invoque_Request(Ses, Pending_Req.Req);

                       when Unknown_Object =>
                         raise GIOP.Error;

                       when Object_Forward =>
                          declare
                             Sock : Socket_Type;
                             TE_Socket : Socket_EndPoint := Lower(Lower(Ses));
                          begin
                             Select_Profile(Ses.Buffer_In, Pend_Req.Profile);
                             Create_Socket (Sock);
                             Connect_Socket (Sock, Pend_Req.Profile.Address);
                             Create (Socket_Endpoint (TE_Socket.all), Sock);
                             Invoque_Request(Ses, Pend_Req.Req);
                          end;
                    end case;
                end;
              else
                  raise GIOP_Error;
              end if;

           when Close_Connection =>
              if Ses.Role = Server or Ses.Minor_Version =2

              else
                raise GIOP_Error;
              end if;

           when Message_Error =>
         end case;
      end;

      S.Expect_Header := True;
      Buffers.Release_Contents (S.Buffer_In.all);
      Expect_Data (S, S.Buffer_In, Message_Header_Size);

      --  Prepare to receive next message.

   end Handle_Data_Indication;



   ------------------------
   -- Utilities Functions
   --------------------------


   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Msg_Type) is
   begin
      Marshall (Buffer, MsgType_To_Octet (Value));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Reply_Status_Type) is
   begin
      Marshall (Buffer, ReplyStatusType_To_Unsigned_Long (Value));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Locate_Status_Type) is
   begin
      Marshall (Buffer, LocateStatusType_To_Unsigned_Long (Value));
   end Marshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Msg_Type is
   begin
      return Octet_To_MsgType (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Reply_Status_Type is
   begin
      return Unsigned_Long_To_ReplyStatusType (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Locate_Status_Type is
   begin
      return Unsigned_Long_To_LocateStatusType (Unmarshall (Buffer));
   end Unmarshall;



end Droopi.Protocols.GIOP

