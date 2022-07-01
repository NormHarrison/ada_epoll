with System;

private with Interfaces;
private with Interfaces.C;

private with GNAT.Sockets;


package Epoll is

   --  ! Compare using an enumeration type only vs. using an array
   --  indexed by an enumertion type,

   type Epoll_Type is limited private;
   --  ! Disriminant?

   --  ! Create an exception to raise on errors.

   procedure Create
     (Epoll         : in out Epoll_Type,
      Close_On_Exec : in     Boolean := False);

   type Event_Kind is
     (EK_Read,            --  EPOLLIN
      EK_Exceptional,     --  EPOLLPRI
      EK_Write,           --  EPOLLOUT
      EK_Read_Normal,     --  EPOLLRDNORM ?
      EK_Read_Band,       --  EPOLLRDBAND ?
      EK_Write_Normal,    --  EPOLLWRNORM ?
      EK_Write_Band,      --  EPOLLWRBAND ?
      EK_Message,         --  EPOLLMSG ?
      EK_Error,           --  EPOLLERR (implicit)
      EK_Closed,          --  EPOLLHUP (implicit)
      EK_Read_Closed,     --  EPOLLRDHUP
      EK_Exclusive,       --  EPOLLEXCLUSIVE (kernel 4.5)
      EK_No_Suspend,      --  EPOLLWAKEUP (kernel 3.5)
      EK_One_Shot,        --  EPOLLONESHOT (kernel 2.6.2)
      EK_Edge_Triggered); --  EPOLLET

   type Event_Kind_Mask is array (Event_Kind) of Boolean with Pack => True;

   type Operation_Kind is (Add, Delete, Modify);

   procedure Control
     (Epoll      : in out Epoll_Type;
      Operation  : in     Operation_Kind;
      Descriptor : in     Integer;
      Mask       : in     Event_Kind_Mask;
      Meta_Data  : in     Long_Long_Integer);


   type Event_Type is private; --  ! Make limited too?

   type Event_Array is array (Positive range <>) of Event_Type;

   function Get_Mask (Event : in Event_Type) return Event_Kind_Mask;

   function Get_Meta_Data (Event : in Event_Type) return Long_Long_Integer;


   function Wait
     (Epoll   : in out Epoll_Type;
      Timeout : in     Duration) return Event_Array;


   procedure Close (Epoll : in out Epoll_Type);
   --  ! We need to provide a `Close` subprogram too.

   --  ! We need to provide a `Abort_Wait` subprogram.


private

   package C renames Interfaces.C;

   for Operation_Kind use (Add => 1, Delete => 2, Modify => 3);

   type Epoll_Type is limited record
      Descriptor     : C.int   := -1;
      Interest_Count : Natural :=  0;

      Read_Sock, Write_Sock : GNAT.Sockets.Socket_Type :=
        GNAT.Sockets.No_Socket;
   end record;

   type Event_Type is record
      Events : Interfaces.Unsigned_32;
      Data   : Long_Long_Integer;
   end record;


end Epoll;
