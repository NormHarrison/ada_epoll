with Interfaces;

with GNAT.OS_Lib;


package body Epoll is

   Event_Kind_Values : constant array (Event_Kind) of Interfaces.Unsigned_32 :=
     (EK_Read           => 16#001#,
      EK_Exceptional    => 16#002#,
      EK_Write          => 16#004#,
      EK_Read_Normal    => 16#040#,
      EK_Read_Band      => 16#080#,
      EK_Write_Normal   => 16#100#,
      EK_Write_Band     => 16#200#,
      EK_Message        => 16#400#,
      EK_Error          => 16#008#,
      EK_Closed         => 16#010#,
      EK_Read_Closed    => 16#2000#,
      EK_Exclusive      => Interfaces.Shift_Left (1, 28),
      EK_No_Suspend     => Interfaces.Shift_Left (1, 29),
      EK_One_Shot       => Interfaces.Shift_Left (1, 30),
      EK_Edge_Triggered => Interfaces.Shift_Left (1, 31));

   --  EPOLL_CLOEXEC value is 02000000.

   ------------
   -- Create --
   ------------

   procedure Create
     (Epoll         : in out Epoll_Type,
      Close_On_Exec : in     Boolean := False)
   is
      function Epoll_Create1 (Flags : in C.int) return C.int with
        Import     => True,
        Convention => C,
        Link_Name  => "epoll_create1";

   begin
      null;
   end Create;

   -------------
   -- Control --
   -------------

   procedure Control
     (Epoll      : in out Epoll_Type;
      Operation  : in     Operation_Kind;
      Descriptor : in     Integer;
      Mask       : in     Event_Kind_Mask;
      Meta_Data  : in     Long_Long_Integer)
   is
      function Epoll_Ctl
        (EpFd  : in C.int;
         Op    : in C.int;
         Fd    : in C.int;
         Event : in System.Address) return C.int --  ! Must be a raw address?
      with
        Import     => True,
        Convention => C,
        Link_Name  => "epoll_ctl";

   begin
   end Control;

   ----------
   -- Wait --
   ----------

   function Wait
     (Epoll   : in out Epoll_Type;
      Timeout : in     Duration) return Event_Array
   is
      function Epoll_Wait
        (EpFd      : in C.int;
         Events    : in System.Address; --  ! Must be a raw address?
         MaxEvents : in C.int;
         Timeout   : in C.int) return C.int
      with
        Import     => True,
        Convention => C,
        Link_Name  => "epoll_wait";

   begin
   end Wait;

   -----------
   -- Close --
   -----------

   procedure Close (Epoll : in out Epoll_Type) is
      use GNAT.OS_Lib;
   begin
      if Epoll.Descriptor /= -1 then
         Close (File_Descriptor (Epoll.Descriptor));
         Close (File_Descriptor (Epoll.Read_Sock));
         Close (File_Descriptor (Epoll.Write_Sock));

         --  ! Reset file descriptors to their defaults?

         Epoll.Interest_Count := 0;
      end if;
   end Close;

end Epoll;
