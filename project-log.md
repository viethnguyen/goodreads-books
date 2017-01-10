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
- To parse XML file, probably use TagSoup? Read this tutorial, it's
  helpful: https://hackage.haskell.org/package/tagsoup

# Dec 31, 2016

- In each Goodread response, each item is wrapped inside a `<review>`
  tag 
- What information do we need for a book? ISBN, book name, book image
  URL, book description, my comments.
- To be easy, save a Goodread response into a file first, then using
  TagSoup on that file. 

# Jan 2, 2017

- Use tagsoup to parse a review to get book info. 
- Probably use `xmlhtml` library is more efficient. 
- Now needs to input book info into a Heist template. In the previous
  version, `Text.Templating.Heist` is used
  (https://hackage.haskell.org/package/heist-0.7.0/docs/Text-Templating-Heist.html). But
  from `Heist 1.0`, the functions are moved to `Heist.Interpreted` (https://hackage.haskell.org/package/heist-1.0.1.0/docs/Heist-Interpreted.html).
- Heist tutorial about Compiled Heist:
  http://snapframework.com/docs/tutorials/compiled-splices. This is
  after version 0.10 
- An example of using interprested Heist:
  https://github.com/ericrasmussen/snap-heist-examples/blob/master/src/handlers/Conditional.hs
  
- See this code, it binds data from code into a Heist template: 
https://github.com/ericrasmussen/snap-heist-examples/blob/master/src/handlers/Loop.hs
and
https://github.com/ericrasmussen/snap-heist-examples/blob/master/snaplets/heist/templates/loop/_tutorials.tpl

# Jan 3, 2017 

- In order to use the operator `(##)`, I need to import
  `Data.Map.Syntax`. 

- Able to parse a Goodreads response to get a collection of book
  infos. But this exists inside an IO, how do I pass this into Snap
  App Handler? 
  
- Probably to read data from file or db, need another approach, see
  the code:
  https://github.com/bonus500/sc2blog/blob/master/src/Site.hs

# Jan 4, 2017 nn

- Still trying to connect data read from file to Snap. 

- Some info:

```haskell 

type Splices s = MapSyntax T.Text s

type SnapletISplice b = SnapletHeist b (Handler b b) Template

renderWithSplices
  :: HasHeist b =>
     ByteString -> Splices (SnapletISplice b) -> Handler b v ()
  	-- Defined in ‘Snap.Snaplet.Heist’

```

- FOUND THE SOLUTION! I only need to use `liftIO` in the bookHandler
  (of type `Handler App App ()`) to "get info out of IO" then pass it
  to the splice function. 
  
- Some bugs need to be fixed: include a css file, remove html tag from
  the goodreads responses. 
  
  
# Jan 5, 2017 

- Fixed the bug of include a css file. I put my css file in the folder
  `static/media/css/main.css`, then I need to specify in Snap code the
  routes accordingly
  
```haskell
routes :: [(ByteString, Handler App App ())]
routes = [ ("/book", bookHandler)
         , ("media", serveDirectory "static/media")]
```

then in Snap HTML template I can specify the path to the css file:

```html
    <link href="media/css/worg.css" rel="stylesheet" type="text/css"
    title="Worg" />
```

- Side note: to deploy to Heroku, see this tutorial: http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html

- To remove html tag from the goodreads response, probably
  `Hakyll.Web.Html` might be good: https://hackage.haskell.org/package/hakyll-4.1.2.1/docs/Hakyll-Web-Html.html

- On second thought, the response doesn't have to be stripped off the
  tag. What I want is that the final render page will display those
  HTML tag correctly. 

- Or take a look at `Web.Encoding`, the decodeHtml
  function. (http://hackage.haskell.org/package/web-encodings-0.3.0.2/docs/Web-Encodings.html)
  
# Jan 6, 2017 

- I tried to strip off tags like `<br/>` by writing a custom TagSoup
  code. But without them, the format of the text looks weird. But it
  seems ok to me. 

- I added the link to Goodreads for each book. 

# Jan 7, 2017 

- I added more comments to the code
- I'm trying to deploy the app to Heroku. This task needs more time.
- I removed the temporary response file after processing. 
- Need to know a bit about Heroku:
- Applications consist of your source code, a description of any
  dependencies, and a Procfile.
- Terminology: A slug is a bundle of your source, fetched
  dependencies, the language runtime, and compiled/generated output of
  the build system - ready for execution.
  
# Jan 10, 2017 

- Try to deploy code to Heroku 

- To load private key to Heroku, use Config Vars:
  https://devcenter.heroku.com/articles/config-vars
  
- On a traditional host or working locally you can set environment
  vars in your `bashrc` file. On Heroku, you use config vars. 
  
- Setting up config vars for a deployed application: use `config:set`,
  `config:get` and `config:unset`.
  
- You can also edit config vars on your app settings tab on
  Dashboard. 
  
- On local machine, the environmental variable can be set by add to
  `~/.zshrc`: 
  
```sh
export GOODREADS_KEY="..."
```


