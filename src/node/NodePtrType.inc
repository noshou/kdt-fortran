!> Pointer to an owned copy of a Node returned by a search.
!! p is always a pointer to a heap-allocated copy; call destroy() or let it
!! go out of scope to free the copy.
type :: NodePtr
    type(Node), pointer     :: p    => null()
    contains
        procedure           :: destroy => destroyNodePtr
        final               :: finalizerNodePtr
end type NodePtr
