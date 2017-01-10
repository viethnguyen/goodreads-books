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
used a very small features of this framework. 

Snap has a scaffolding tool to quickly get users started. If you are
new to Snap, start with `Site.hs` and `Application.hs`. In this
project, I only added code to `Site.hs`. 

Documents for Snap:


## Deploy to Heroku 
