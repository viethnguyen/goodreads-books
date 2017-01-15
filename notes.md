# Development notes 

I keep this app simple: connect to my Goodreads using its API, retrieve
the list of books I read, then program Snap to serve a page with a
table displaying book info (book cover, title, author, Goodreads
description, and my comments). 

## Connect to my Goodreads book list 

Goodreads requires registering API key in order to access their
APIs. After doing so, I saved the API key as an environmental
variable.

- In my local machine, add to `.zshrc`: 

```sh
export GOODREADS_KEY="..."
```

- In Heroku, use Heroku CLI to add config vars:

```sh
$ heroku config:ste GOODREADS_KEY=...
```

In the code, this API key is read by using `getEnv` in
`System.Environment`: 

```haskell
goodreadsKey :: IO String
goodreadsKey = getEnv "GOODREADS_KEY"
```

## Serve a webpage using Snap

Snap is a mature web framework in Haskell. In this project, I only
used a very small subset of features in this framework. 

Snap has a scaffolding tool to quickly get users started. If you are
new to Snap, start with `Site.hs` and `Application.hs`. In this
project, I only added code to `Site.hs`. 

To define app routes: 

```haskell
routes :: [(ByteString, Handler App App ())]
routes = [ ("/book", bookHandler)
         , ("media", serveDirectory "static/media")]
```

Snap uses Heist templating framework. 

## Parse Goodreads response 

The application saves Goodreads response in an inner file. This file
is updated periodically in a separate thread so that the data is
always fresh.

To parse this response file, I use `tagsoup` library. The fields I'm
interested in includes: book cover image link, book name, author, book
description and my comment. I extracted these fields by mostly using
`takeWhile` and `dropWhile` functions in `tagsoup` to extract info
between specific tags. 


## Deploy to Heroku 

I read a little bit about how Heroku works. Basically, an application
in Heroku consists of the source code, a description of any
dependencies, and a Procfile. 

There is a CLI tool to install on local machine to interact with
remote Heroku server. 

Typically, the way to deploy app to Heroku is that we need to push the
code into a Heroku git repo. Once it's done, the code will be compiled
on the Heroku server side. App on Heroku relies on Buildpack to tell
it how to compile the code. For this Haskell
app, I use this Buildpack:
https://github.com/begriffs/heroku-buildpack-ghc.git. I needed to specify
it in the Settings of the app in Heroku control panel. 

In this application, there is a private key needs to be stored in the
app. Heroku supports loading private key with `Config Vars`: https://devcenter.heroku.com/articles/config-vars

In Procfile, I need to specify how to start the server: 

```sh
web: dist/build/goodreads-books/goodreads-books -p $PORT
```


