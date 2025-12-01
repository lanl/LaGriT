## Jekyll

The LaGriT site uses Jekyll which is a simple, blog-aware, static site generator perfect for personal, project, or organization sites. Think of it like a file-based CMS, without all the complexity. Jekyll takes your content, renders Markdown and Liquid templates, and spits out a complete, static website ready to be served. Jekyll is the engine behind GitHub Pages, which you can use to host sites from your GitHub repositories.

### Installation

One of the benifits to Jekyll is that you can view your site edits locally prior to commiting them and making them live. You can find detailed information for installation at [https://jekyllrb.com/docs/installation/](https://jekyllrb.com/docs/installation/).

### Development

This folder contains a _config.yml file which contains settings that control your site. It is not usually updated unless there is a major theme update. Any changes to this file will require you to restart the server process (see below).

This folder also contains a Gemfile. This is where you manage which Jekyll version is used to run. When you want to use a different version, change it in your Gemfile, save the file and run `bundle install`. Then restart the server. This will help ensure the proper Jekyll version is running.

There is a Gemfile.lock. This file should not be modified unless you really know what you are doing. It contains all of the versions for the necessary dependencies.

#### Starting the Jekyll server

To start your server you should ensure that you are in the /docs folder and then run

```
bundle exec jekyll serve
```

This will start the server and make it viewable on your localhost. The address will be given on the terminal. For example:

```
Server address: http://127.0.0.1:4000
Server running... press ctrl-c to stop.
```

Prior to pushing updates to Github, you should preform a build.

```
bundle exec jekyll build
```