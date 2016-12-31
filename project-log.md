# Resources

- Snap Hackage page: https://hackage.haskell.org/package/snap
- Heist Hackage page:
  https://hackage.haskell.org/package/snap-1.0.0.1/docs/Snap-Snaplet-Heist.html
  

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
- Some Snap resources that need to read:
  + http://bonus500.github.io/sc2blog/#title-slide
  + https://github.com/j-hannes/snap-tutorial
  + Github repo: https://github.com/snapframework/snap
- If you are new to Snap, start with `Site.hs` and `Application.hs`
- Several things need to read more: Splice, Heist template, CSS template
- Heist tutorial: http://snapframework.com/docs/tutorials/heist
- Example of `bind` in Heist: 

```html
<bind tag="longname">
      Einstein, Feynman, Heisenberg, and Newton Research Corporation
      Ltd.<sup>TM</sup>
</bind>
<p>
	We at <longname/> have research expertise in many areas of physics.
    Employment at <longname/> carries significant prestige.  The rigorous
    hiring process developed by <longname/> is leading the industry.
</p>	   
```

- The `apply` tag: loads one of the application templates and inserts
  it into the current template's node tree. If the target template
  does not have any special tags, then the contents of the `apply` tag
  are ignored. 
- Heist lets you bind tags to Haskell code with a splice. A `Splice`
  takes the input node from the template and outputs a list of nodes
  that get "spliced" back into the template. 

# Dec 30, 2016

- Explore how to get info from Goodreads using its API.
- See this: https://www.goodreads.com/api/
- "Get the books on members shelf"
- We can get all information about the books by this HTTP GET: 

```sh
$ curl
"https://www.goodreads.com/review/list/5285276.xml?key=jA5jRDRqX6LFkhCkZCppmQ&v=2?shelf=read"
```

This query get all information about the books in the `read` shelf. 
- To do HTTP client, `wreq` is an option for Haskell. 
- How to pass the response from wreq to the main program? 

