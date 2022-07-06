private with Interfaces;
private with Interfaces.C;

private with GNAT.Sockets;


package Epoll is

   type Epoll_Type is limited private;
   --  ! Disriminant?

   type Epoll_Access is access all Epoll_Type;

   Epoll_Error : exception;

   procedure Create
     (Epoll         : in out Epoll_Type;
      Close_On_Exec : in     Boolean := False);


   type Event_Kind_Type is private;

   function "+" (Left, Right : in Event_Kind_Type) return Event_Kind_Type;
   function "&" (Left, Right : in Event_Kind_Type) return Boolean;

   EK_Read           : constant Event_Kind_Type; --  EPOLLIN
   EK_Exceptional    : constant Event_Kind_Type; --  EPOLLPRI
   EK_Write          : constant Event_Kind_Type; --  EPOLLOUT
   EK_Read_Normal    : constant Event_Kind_Type; --  EPOLLRDNORM ?
   EK_Read_Band      : constant Event_Kind_Type; --  EPOLLRDBAND ?
   EK_Write_Normal   : constant Event_Kind_Type; --  EPOLLWRNORM ?
   EK_Write_Band     : constant Event_Kind_Type; --  EPOLLWRBAND ?
   EK_Message        : constant Event_Kind_Type; --  EPOLLMSG ?
   EK_Error          : constant Event_Kind_Type; --  EPOLLERR (implicit)
   EK_Closed         : constant Event_Kind_Type; --  EPOLLHUP (implicit)
   EK_Read_Closed    : constant Event_Kind_Type; --  EPOLLRDHUP
   EK_Exclusive      : constant Event_Kind_Type; --  EPOLLEXCLUSIVE (kernel 4.5)
   EK_No_Suspend     : constant Event_Kind_Type; --  EPOLLWAKEUP (kernel 3.5)
   EK_One_Shot       : constant Event_Kind_Type; --  EPOLLONESHOT (kernel 2.6.2)
   EK_Edge_Triggered : constant Event_Kind_Type; --  EPOLLET


   type Operation_Kind is (Add, Delete, Modify);

   subtype Meta_Data_Range is Integer range Integer'First + 1 .. Integer'Last;
   --  ! Make a `new` type instead?

   procedure Control
     (Epoll      : in out Epoll_Type;
      Operation  : in     Operation_Kind;
      Descriptor : in     Integer;
      Event_Mask : in     Event_Kind_Type;
      Meta_Data  : in     Meta_Data_Range) with Inline;


   type Event_Type is private;

   type Event_Array is array (Positive range <>) of Event_Type;
   --  ! Use convetion "C" ?...

   function Get_Meta_Data (Event : in Event_Type) return Meta_Data_Range;

   function Get_Event_Mask (Event : in Event_Type) return Event_Kind_Type;

   type Milliseconds is new Integer range -1 .. Integer'Last;
   for Milliseconds'Size use 32;

   Forever   : constant Milliseconds;
   Immediate : constant Milliseconds;

   function Wait
     (Epoll   : in out Epoll_Type;
      Timeout : in     Milliseconds) return Event_Array;


   procedure Close (Epoll : in out Epoll_Type);


   procedure Abort_Wait (Epoll : in out Epoll_Type);


private

   package C renames Interfaces.C;

   use type C.int;

   Read_Sock_Unique_ID : constant Integer := Integer'First;

   type Atomic_Boolean is new Boolean with
     Atomic        => True,
     Default_Value => False;

   type Epoll_Type is limited record
      Descriptor     : C.int   := -1;
      Interest_Count : Natural :=  0;
      --  ! How many descriptors can epoll actually watch?

      Is_Aborting : Atomic_Boolean;
      Was_Aborted : Atomic_Boolean;
      Is_Waiting  : Atomic_Boolean;

      Read_Sock  : GNAT.Sockets.Socket_Type := GNAT.Sockets.No_Socket;
      Write_Sock : GNAT.Sockets.Socket_Type := GNAT.Sockets.No_Socket;
   end record;

   type Event_Kind_Type is new Interfaces.Unsigned_32;

   EK_Read           : constant Event_Kind_Type := 16#001#;
   EK_Exceptional    : constant Event_Kind_Type := 16#002#;
   EK_Write          : constant Event_Kind_Type := 16#004#;
   EK_Read_Normal    : constant Event_Kind_Type := 16#040#;
   EK_Read_Band      : constant Event_Kind_Type := 16#080#;
   EK_Write_Normal   : constant Event_Kind_Type := 16#100#;
   EK_Write_Band     : constant Event_Kind_Type := 16#200#;
   EK_Message        : constant Event_Kind_Type := 16#400#;
   EK_Error          : constant Event_Kind_Type := 16#008#;
   EK_Closed         : constant Event_Kind_Type := 16#010#;
   EK_Read_Closed    : constant Event_Kind_Type := 16#2000#;
   EK_Exclusive      : constant Event_Kind_Type := 16#10000000#;
   EK_No_Suspend     : constant Event_Kind_Type := 16#20000000#;
   EK_One_Shot       : constant Event_Kind_Type := 16#40000000#;
   EK_Edge_Triggered : constant Event_Kind_Type := 16#80000000#;

   for Operation_Kind use (Add => 1, Delete => 2, Modify => 3);

   type Event_Type is record
      Event_Mask : Event_Kind_Type; --  `Events`
      Meta_Data  : Integer;         --  `Data`
   end record;

   Forever   : constant Milliseconds := -1;
   Immediate : constant Milliseconds :=  0;

end Epoll;
