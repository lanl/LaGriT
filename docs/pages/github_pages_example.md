---
title: "Example LaGriT Page"
author: My Name
tags: example, quickstart
categories: quickstart
---


<style>
/*
Style taken from Github's Mastering Markdown document
*/
.markdown-example {
  margin: 20px 0;
  border: 1px solid #ccc;
  border-radius: 3px;
  box-shadow: 0 1px 3px rgba(0,0,0,.075);
}
.markdown-example .source {
  padding: 20px;
  margin: 0;
  font-size: 16px;
  line-height: 28px;
  white-space: pre-wrap;
  background: #F8F8F8;
  border: none;
  border-bottom: 1px solid #ccc;
  border-radius: 0;
}
.markdown-example .rendered {
  padding: 20px;
}
</style>

# LaGriT Style Guide #

------------------------------------

In an effort to maintain consistency across the GitHub Pages implementation of LaGriT documentation, this document is a style guide (draft). Its use should be applied across all commands. See [the ADDMESH page](docs/commands/ADDMESH.md) for an example of it applied.

This page uses HTML to format box sections to show raw text and the resulting format. Use 

```
<div class="markdown-example" id="example-text">
<pre class="source">
sample format written to show html text
</pre>
<div class="rendered">
sample format written to show result
</div>
</div>
```

## Table of Contents 

1. [Page Titles and Headers](#pagetitles)
2. [Command Formatting](#cmdformatting)
3. [Inline Code](#inlinecode)
4. [Linking to Other Pages](#linking)
5. [Embedding Images](#embeddingimages)
6. [Further Resources](#resources)

## 1. Page Headers & Titles <a name="pagetitles"></a>

Each page should contain YAML-formatted metadata at the top of the document.
See [GitHub's announcement](https://blog.github.com/2013-09-27-viewing-yaml-metadata-in-your-documents/) and a [basic guide on supported tags](https://hackmd.io/s/yaml-metadata#tags).

In general, most metadata will do nothing.
However, each document should contain at least these specifiers:

1. `title` - this renders as the browser page title, and renders in search
2. `tags` - this benefits the search functionality as well

<div class="markdown-example" id="example-text">
<pre class="source">
---
title: "Example LaGriT Page"
author: My Name
tags: example, quickstart, style
---
</pre>
</div>

Page titles should use the `h1` header class, note the # character must not have spaces before it.
This is accomplished in Markdown by:

<div class="markdown-example" id="example-text">
<pre class="source">
# Page Title 
</pre>
<div class="rendered">
<h1>Page Title</h1>
</div>
</div>


Subsections should be marked with `h2`, subsubsections with `h3`, etc.

<div class="markdown-example" id="example-text">
<pre class="source">
# Command 
--------------------

## 1. Usage 
### 1.1 First Step 
This is the first step to using this great command.

### 1.2 Second Step 
This is the second step to using this command.
</pre>
<div class="rendered">
<h1>My Command</h1>
<hr/>
<h2>1. Usage</h2>
<h3>1.1 First Step</h3>
  <p>This is the first step to using this great command.<br></p>
<h3>1.2 Second Step</h3>
  <p>This is the second step to using this command.</p>
</div>
</div>

## 2. Formats for Command Syntax <a name="cmdformatting"></a>

When rendering LaGriT commands, in general, use **`bold monospaced font`** for commands, **bold** for literal keywords and `unstyled monospace` for primary variable names, and *`italic variable`* for primary mesh objects, etc.

| :--------------------------- | :------------------------------------ |
| **`bold command`**            |```**`bold command`**```|
| **bold literal**              |```**bold literal**```  |
| `unstyled variable`           |````unstyled variable````|
| *`italic variable`*           |```*`italic variable`*```|



Some formatting is difficult to do in pure Markdown, but fortunately very easy to do in HTML.
Markdown specification includes embedded HTML parsing. 

LaGriT command SYNTAX should be styled as:

<div class="markdown-example" id="example-text">
<!--
GH Pages keeps trying to render this as HTML instead of raw code.
So, we're going to have to escape the HTML tags to get them literally rendered...
-->
<pre class="source">
&lt;pre&gt;
&lt;b&gt;extract / surfmesh&lt;/b&gt; / 1,0,0 / cmo_out / [cmo_in] / [&lt;b&gt;external&lt;/b&gt;]
&lt;/pre&gt;
</pre>
<div class="rendered">
<pre>
<b>extract / surfmesh</b> / 1,0,0 / cmo_out / [cmo_in] / [<b>external</b>]
</pre>
</div>
</div>

Note how the block in enclosed in the `<pre>` tags.

## 3. Inline Code <a name="inlinecode"></a>

Similarily to the section above, for sections(s) of code within a larger normally typed paragraph, literal keywords should be bold and all other objects should be unstyled. 

<div class="markdown-example" id="example-text">
<pre class="source">
Similar to merge except `imt`, `icr`, `itetclr` of `mesh2` have the value max(imt(mesh1)) added to `mesh2`.
</pre>
<div class="rendered">
<p>Similar to merge except <b>imt</b>, <b>icr</b>, <b>itetclr</b> of <code>mesh2</code> have the value max(imt(mesh1)) added to <code>mesh2</code>.</p>
</div>
</div>

## 4. Linking to Other Pages <a name="linking"></a>

You can link to other pages on this site by either using the literal URL, or (more appropriately) with a relative link to the corresponding Markdown file. It is better practice to use a relative link to a Markdown file, so that those attempting to read the documentation through a Markdown viewer (i.e. after cloning the LaGriT repo) will be able to easily access the referenced document.

<div class="markdown-example" id="example-text">
<pre class="source">
As an example, [this page](https://lanl.github.io/LaGriT/pages/docs/commands/ADDMESH.html)
will take you to the same place as [clicking here](docs&#x2F;commands&#x2F;ADDMESH.md).
</pre>
<div class="rendered">
<p>As an example, <a href="https://lanl.github.io/LaGriT/pages/docs/commands/ADDMESH.html">this page</a> will take you to the same place as <a href="https://lanl.github.io/LaGriT/pages/docs/commands/ADDMESH.html">clicking here</a>.</p>
</div>
</div>


## 6. Miscellaneous 

### 6.1 Inline Text Formatting

<div class="markdown-example" id="example-text">
<pre class="source">
This is **bold**, *italics*, ~~strikthrough~~, and `code`.
You can stack these, like:
**`bold code`**, *`italic code`*, ~~`strikethrough_code`~~.
</pre>
<div class="rendered">
  This is <strong>bold</strong>, <i>italics</i>, <s>strikthrough</s>, and <code>code</code>.
You can stack these, like:

**`bold code`**, *`italic code`*
</div>
</div>

### 6.2 Embedding Images

<div class="markdown-example" id="example-text">
<pre class="source">

Below are multiple ways to embed images in a Markdown file.
Note that relative paths are relative to LaGriT/docs/pages/docs/commands/.

&lt;img width=&quot;500&quot; src=&quot;https://lanl.github.io/LaGriT/assets/images/sphere_cube_colors26_expand.png">
&#x0021;&#x005b;alternate text](https://lanl.github.io/LaGriT/assets/images/sphere_cube_colors26_expand.png)
&lt;img src=&quot;&#123;&#123;'assets/images/sphere_cube_colors26_expand.png' | absolute_url&#125;&#125;&quot; width=&quot;500&quot;>
&#x0021;&#x005b;alternate text](../../../assets/images/sphere_cube_colors26_expand.png)
&#x0021;&#x005b;alternate text](&#123;&#123; site.baseurl }}/assets/images/sphere_cube_colors26_expand.png)
&#x0021;&#x005b;alternate text](&#123;&#123;'assets/images/sphere_cube_colors26_expand.png' | absolute_url&#125;&#125;)
</pre>
<div class="rendered">
<img width="200" src="https://lanl.github.io/LaGriT/assets/images/sphere_cube_colors26_expand.png">
</div>
</div>

## 7. Further Resources <a name="resources"></a>

Markdown supports tables, lists, embedded HTML/Javascript/CSS, images, and more.
For syntax usage on embedding these elements, a few references can be found below:

* [Markdown Quick Cheatsheet (Common Mark)](https://commonmark.org/help/)
* [Mastering Markdown (GitHub)](https://guides.github.com/features/mastering-markdown/) - basic Markdown syntax overview
* [Markdown Cheatsheet (assemble.io)](http://assemble.io/docs/Cheatsheet-Markdown.html) - discusses the relationship between Markdown elements and HTML
