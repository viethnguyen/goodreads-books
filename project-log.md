# Dec 28, 2016

- Start the project 
- Web framework to use: Snap
- To connect to Goodreads, use what library? 
- Snap is now version 1.0 and stable. 
- There are a collection of Snaplets available here:
  http://hackage.haskell.org/packages/#cat:Snap

# Dec 29, 2016 
- All calls to child snaplet initializer func. must be wrapped in a
  call to `nestSnaplet`. The first param is a URL path segment that is
  used to prefix all routes defined by the snaplet. If you want a
  snaplet's routes to be available at the top level, just pass an
  empty string to `nestSnaplet`. The second param is the lens to the
  snaplet you're nesting. 
- `nameSnaplet`: the author of a snaplet defines a default name for
  the snaplet in the first argument to the makeSnaplet function. This
  name is used for the snaplet's directory in the filesystem. If you
  don't want to use the default name, you can override it with the
  `nameSnaplet` function. 
- `addRoutes`: define how an application (or snaplet) defines its
  routes. Under the hood, the snaplet infrastructure merges all the
  routes from all snaplets, prepends prefixes from `nestSnaplet`
  calls, and passes the list to Snap's `route` function. 
- `wrapSite`: allows you to apply an arbitrary `Handler`
  transformation to the top-level handler. This is useful if you want
  to do some generic processing at the beginning or end of every
  request. 
- `with`: accompanies a snaplet with a call to a function. 
