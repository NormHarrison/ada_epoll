### Ada epoll thick-binding

This package contains a thick, yet still simple binding to Linux's epoll
interface for watching activity on file descriptors.

In an attempt to provide a cleaner/simpler interface, certain more-rare
functionality isn't available yet. See TODO.md for such unavailable functionality.

For a usage example, see the `src/example` directory, all documentation is
apart of the package specification file (`epoll.ads`). More in-depth
information about the underlying epoll C interface can be found in the epoll
manual pages.
