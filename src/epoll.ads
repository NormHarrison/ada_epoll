private with Interfaces;
private with Interfaces.C;

private with GNAT.Sockets;


package Epoll is

   --  Represents an epoll instance.
   --  ! Instantiating an instance of this type from an existing epoll
   --    descriptor is currently not implemented.
   --  ! Add a disriminant?

   type Epoll_Type is limited private;

   type Epoll_Access is access all Epoll_Type;

   Epoll_Error : exception;
   --  The single exception raised by all subprograms when they encounter
   --  epoll-related errors. Exceptions are raised whenever an underlying
   --  epoll C function returns a value of -1. The exception message will
   --  contain the error code inside the C `errno` variable and its
   --  corresponding description.

   procedure Create
     (Epoll         : in out Epoll_Type;
      Close_On_Exec : in     Boolean := False);
   --  Creates a new epoll instance in kernel memeory, the optional parameter
   --  `Close_In_Exec` prevents sub-processes of the program that created the
   --  instance from inheriting the descriptor that represents the epoll
   --  instance. Raises `Epoll_Error` on failure to create a new epoll
   --  instance.


   --  `Event_Kind_Type` values are used to determine what events will cause
   --  notification of activity on a specific file descriptor, they tell the
   --  kernel what were interested in knowing about.

   type Event_Kind_Type is private;

   EK_None           : constant Event_Kind_Type; --  Only implicit events.
   EK_Read           : constant Event_Kind_Type; --  EPOLLIN
   EK_Exceptional    : constant Event_Kind_Type; --  EPOLLPRI
   EK_Write          : constant Event_Kind_Type; --  EPOLLOUT
   EK_Read_Normal    : constant Event_Kind_Type; --  EPOLLRDNORM
   EK_Read_Band      : constant Event_Kind_Type; --  EPOLLRDBAND
   EK_Write_Normal   : constant Event_Kind_Type; --  EPOLLWRNORM
   EK_Write_Band     : constant Event_Kind_Type; --  EPOLLWRBAND
   EK_Message        : constant Event_Kind_Type; --  EPOLLMSG
   EK_Error          : constant Event_Kind_Type; --  EPOLLERR (implicit)
   EK_Closed         : constant Event_Kind_Type; --  EPOLLHUP (implicit)
   EK_Read_Closed    : constant Event_Kind_Type; --  EPOLLRDHUP
   EK_Exclusive      : constant Event_Kind_Type; --  EPOLLEXCLUSIVE (kernel 4.5)
   EK_No_Suspend     : constant Event_Kind_Type; --  EPOLLWAKEUP (kernel 3.5)
   EK_One_Shot       : constant Event_Kind_Type; --  EPOLLONESHOT (kernel 2.6.2)
   EK_Edge_Triggered : constant Event_Kind_Type; --  EPOLLET

   function "+" (Left, Right : in Event_Kind_Type) return Event_Kind_Type;
   --  Indicate interest in an event kind.

   function "&" (Left, Right : in Event_Kind_Type) return Boolean;
   --  Check for interest in an event kind.


   --  `Event_Type` instances hold user-specified data that was previously
   --  tied to a particular file descriptor when added to an epoll instance's
   --  interest list. This user-specified data can be retrieved via the two
   --  provided functions.

   type Event_Type is private;

   type Event_Array is array (Positive range <>) of Event_Type;
   --  ! Use convetion "C" ?...


   --  `Meta_Data_Range` values represent user-specific data that is tied to
   --  an event during a call to `Add` or `Modify` and which will be provided
   --  alongside the file descriptor it's paired to whenever it has activity.

   subtype Meta_Data_Range is Integer range Integer'First + 1 .. Integer'Last;
   --  ! Make a `new` type instead?

   function Get_Meta_Data (Event : in Event_Type) return Meta_Data_Range;
   --  Returns the meta data that was tied to the file descriptor on which
   --  the activity occurred.

   function Get_Event_Mask (Event : in Event_Type) return Event_Kind_Type;
   --  Returns the event mask that was tied to the file descriptor on which
   --  the activity occurred.


   procedure Add
     (Epoll      : in out Epoll_Type;
      Descriptor : in     Integer;
      Event_Mask : in     Event_Kind_Type;
      Meta_Data  : in     Meta_Data_Range);
   --  Add file descriptor `Descriptor` with the user-specified data
   --  `Meta_Data` to the interest list of the epoll instance `Epoll`,
   --  providing notification of the event kinds specified in `Event_Mask`.
   --  GNAT packages with types that use file descriptors underneath usually
   --  provide ways of obtaining the underlying descriptor, for example the
   --  `To_C` function of `GNAT.Sockets`, or the public `File_Descriptor` type
   --  of `GNAT.OS_Lib` which can safely casted into a regular `Integer`.
   --  Raises `Epoll_Exception` on failure to add the descriptor to the epoll
   --  instance (for example if the file descriptor has already been added).

   procedure Delete (Epoll : in out Epoll_Type; Descriptor : in Integer);
   --  Deletes file descriptor `Descriptor` from the interest list of the
   --  epoll instance `Epoll`. Raises `Epoll_Exceptions` on failures to
   --  remove the descriptor from the epoll instance (for example, if the
   --  descriptor isn't in the interest list). Keep in mind that the external
   --  closure of a file descriptor (like via `GNAT.OS_Lib.Close`) will
   --  cause a previously added file descriptor to be removed from the
   --  interest list.

   procedure Modify
     (Epoll      : in Epoll_Type;
      Descriptor : in Integer;
      Event_Mask : in Event_Kind_Type;
      Meta_Data  : in Meta_Data_Range);
   --  Updates the event mask and meta data of the file descriptor `Descriptor`
   --  to be `Event_Mask` and `Meta_Data`, respectively. Raises `Epoll_Error`
   --  on failure to modify the event mask or meta data of the descriptor (for
   --  example if descriptor isn't in the interest list). Keep in mind that the
   --  external closure of a file descriptor (like via `GNAT.OS_Lib.Close`)
   --  will cause a previously added file descriptor to be removed from the
   --  interest list.


   --  `Milliseconds` values represent the max amount of time that a call to
   --  `Wait` should block for.

   type Milliseconds is new Integer range -1 .. Integer'Last;
   for Milliseconds'Size use 32;

   Forever   : constant Milliseconds := -1;
   --  A call to `Wait` should never timeout and onyl return once activity
   --  occurs on a file descriptor in the interest list of the epoll instance.

   Immediate : constant Milliseconds := 0;
   --  A call to `Wait` should return immediately even if there no activity has
   --  occurred on a file descriptor in the interest list of the epoll
   --  instance.

   function Wait
     (Epoll   : in out Epoll_Type;
      Timeout : in     Milliseconds) return Event_Array;
   --  Wait for `Timeout` milliseconds for activity to occur on one or
   --  or more of the file descriptors previously added to the interest list
   --  via a call to `Add`. The returned `Event_Array` instance will be a null
   --  array if a timeout occurred or if the call to `Wait` was aborted via a
   --  call to `Abort_Wait` in another task. Raises `Epoll_Error` on failure to
   --  wait for activity to occur (for example if the process received an
   --  interrupt signal).

   procedure Abort_Wait (Epoll : in out Epoll_Type);
   --  Abort (forcefully unblock) a blocked call to `Wait` occurring in another
   --  task. A call to this procedure does nothing if the epoll instance
   --  `Epoll` isn't currently waiting for activity to occur.

   procedure Close (Epoll : in out Epoll_Type);
   --  Frees the kernel's resources tied to this epoll instance and all
   --  other internal file descriptors.


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

   EK_None           : constant Event_Kind_Type := 0;
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

   type Event_Type is record
      Event_Mask : Event_Kind_Type; --  `Events`
      Meta_Data  : Integer;         --  `Data`
   end record;

end Epoll;
