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

In an effort to maintain consistency across the GitHub Pages implementation of LaGriT documentation, this document is a style guide for readers and developers.

This page uses HTML to format box sections to show raw text and the resulting format. Use 

```
<div class="markdown-example" id="example-text">
<pre class="source">sample format written to show the raw page text
</pre>
<div class="rendered">sample to show how format will appear on page
</div>
</div>
```
<div class="markdown-example" id="example-text">
<pre class="source">&lt;b&gt;sample&lt;/b&gt; format written to show the raw page text
</pre>
  <div class="rendered"><b>sample</b> to show how format will appear on page
</div>
</div>

## Table of Contents 

1. [Page Titles and Headers](#pagetitles)
2. [Command Formatting](#cmdformatting)
3. [Inline Code, Examples, Demos](#inlinecode)
4. [Linking to Other Pages](#linking)
5. [Long Page Lists](#longpagelists)
6. [Embedding Images](#embeddingimages)
7. [Further Resources](#resources)

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

Page titles should use the `h1` header class followed by a page line, note the # character must not have spaces in front.
Subsections should be marked with `h2`, subsubsections with `h3`, etc.

<div class="markdown-example" id="example-text">
<pre class="source">

# COMMAND or Page Title

------------------------------------

Page text

## Section Header

</pre>
<div class="rendered">
<h1>COMMAND or Page Title</h1>
<hr>
  
Page text
  
<h2>Section Header</h2>
</div>
</div>

## 3. Command Syntax and Descriptions <a name="cmdformatting"></a>

The LaGriT command syntax section should be enclosed in the <pre> tags. Use the HTML bold formats for the command name and any literal keywords. 

<div class="markdown-example" id="example-text">
<!--
GH Pages keeps trying to render this as HTML instead of raw code.
So, we're going to have to escape the HTML tags to get them literally rendered...
-->
<pre class="source">
&lt;pre&gt;
&lt;b&gt;extract/surfmesh&lt;/b&gt;/1,0,0/cmo_out/ [cmo_in] [&lt;b&gt;external&lt;/b&gt;]
&lt;/pre&gt;
</pre>
<div class="rendered">
<pre>
<b>extract/surfmesh</b>/1,0,0/cmo_out/ [cmo_in] [<b>external</b>]
</pre>
</div>
</div>

In the sections describing words and behavior of the command, the following formats should be followed:

In general, use **`bold monospaced font`** for commands and methods, **bold** for keywords or reserved words, and `unstyled monospace` for primary variable names. The italics  *`italic variable`* as needed for additional variables. The general *italic* is for use in text for notes or references.

The following are used for a command syntax and description:

| Appearance | Markdown Text |
| :--------------------------- | :------------------------------------ |
| [**`COMMAND link`**]()            |``` [**`COMMAND`**](file.md) ```|
| **`COMMAND`**            |``` **`COMMAND`** ```|
| **`method`**             |``` **`method`** ```|
| `variable`                    |``` `variable` ```|
| **keyword**              |``` **keyword** ```  |
| [**keyword_link**]()              |``` [**keyword_link**](file.md) ```  |
| [plain link]()              |``` [plain link](file.md) ```  |
| *`italic variable`*           |``` *`italic variable`* ```|
| *italic text*                 |``` *italic text* ```|



## 3. Inline Code, Examples, Demos <a name="inlinecode"></a>

Command pages may include additional sections. Any inline Code, Examples, or Demos use fenced code blocks and should be wrapped in three backticks. The backticks will format much the same way as the HTML <pre> tags. 

```
This is an inline code block with 3 backticks before and after.
```


To display screen output, use the tag <pre class="lg-output"> instead of backtics or other <pre> tags.

<pre class="lg-output">
This is a block for screen output.
</pre>

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

## 5. Long Page Lists <a name="longpagelists"></a>

For pages with a long list of command options, use a clickable table at the top.
See examples at <a href="https://lanl.github.io/LaGriT/pages/docs/commands/DUMP2.html">dump</a>

Table format with page links. The colon on left will make cells left justified. Use colons on both sides to center:

```
|   |  |  |  |
| :---- |  :----  | :---- |  :---- |
| [**`chad`**](#chad) | [**`coord`**](#coord) | [**`datex`**](#datex) | [**`elem_adj_node`**](#elem_adj) |

...

### **`chad`** <a name="chad"></a>

```

## 6. Embedding Images <a name="embeddingimages"></a>

<div class="markdown-example" id="example-text">
<pre class="source">

Below are multiple ways to embed images in a Markdown file.
Note that relative paths are relative to LaGriT/docs/pages/docs/commands/.
Set Image sizes for a page between 250 and 500. 

Embed with html 
&lt;img width=&quot;500&quot; src=&quot;https://lanl.github.io/LaGriT/assets/images/cube.png">

Using href will make a small image clickable to the full image:
&lt;a href=&quot;cube.png">  &lt;img width=&quot;300&quot; src=&quot;cube.png">  &lt;/a>

Embed with markdown
&#x0021;&#x005b;alternate text](https://lanl.github.io/LaGriT/assets/images/cube.png)

&lt;img src=&quot;&#123;&#123;'assets/images/cube.png' | absolute_url&#125;&#125;&quot; width=&quot;500&quot;>
&#x0021;&#x005b;alternate text](../../../assets/images/cube.png)
&#x0021;&#x005b;alternate text](&#123;&#123; site.baseurl }}/assets/images/cube.png)
&#x0021;&#x005b;alternate text](&#123;&#123;'assets/images/cube.png' | absolute_url&#125;&#125;)
</pre>
<div class="rendered">
<img width="200" src="https://lanl.github.io/LaGriT/assets/images/cube.png">
</div>
</div>

## 7. Further Resources <a name="resources"></a>

Markdown supports tables, lists, embedded HTML/Javascript/CSS, images, and more.
For syntax usage on embedding these elements, a few references can be found below:

* [Markdown Quick Cheatsheet (Common Mark)](https://commonmark.org/help/)
* [Mastering Markdown (GitHub)](https://guides.github.com/features/mastering-markdown/) - basic Markdown syntax overview
* [Markdown Cheatsheet (assemble.io)](http://assemble.io/docs/Cheatsheet-Markdown.html) - discusses the relationship between Markdown elements and HTML
