### Todo

- Allow instantiating/creating an instance of `Epoll_Type` from an existing
epoll descriptor (like when inheriting the descriptor from a parent process).
The main problem with this, is that `Epoll_Type` keeps an internal interest
count automatically, which won't be correct if additions/deletions of
descriptors from the instances interest list were done elsewhere.

- The above problem with the internal interest list also appears  when closing
a file descriptor via external means before formally deleting it via a call to
`Delete`, as closure of a descriptor removes it implicitly from the kernel's
interest list. The only solution we can think of for this at the moment, is
providing a separate `Wait` function that allows the user to explicitly
indicate how many descriptors they would like events for.

- Add a `To_Error`/`To_Code`/`Resolve_Exception` function that converts
exception error codes provided via an exception message into an enumeration
type value (like what `GNAT.Sockets` offers).
