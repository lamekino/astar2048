A* 2048
--------------------------------------------------------------------------------

So I had some code from when I was in college working in some AI class and
thought that it would be a "good" idea to write the A* project in Haskell.
It was wasn't; Haskell's `Data.Array` uses O(N) to copy on every array
modification without the [ST monad](
https://hackage.haskell.org/package/base-4.21.0.0/docs/GHC-Arr.html#t:STArray
) which I didn't know at the time.

Anyways this project tries to fix that and use that A* implementation to play
2048. 😜

--------------------------------------------------------------------------------
*No LLM generated code was used in the creation of this project.*
