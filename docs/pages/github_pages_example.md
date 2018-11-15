---
title: "Example LaGriT Page"
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

In an effort to maintain consistency across the GitHub Pages implementation of LaGriT documentation, this document is a style guide (draft).

## Table of Contents ##

1. [Page Titles](#pagetitles)
2. [Command Formatting](#cmdformatting)
3. sdf
4. sdf

## 1. Page Titles & Headers <a name="pagetitles"></a>

Page titles should use the `h1` header class followed by a horizontal rule.
This is accomplished in Markdown by:

<div class="markdown-example" id="example-text">
<pre class="source">
# Page Title #
--------------------
</pre>
<div class="rendered">
<h1>Page Title</h1>
<hr/>
</div>
</div>

Subsections should be marked with `h2`, subsubsections with `h3`, etc.

<div class="markdown-example" id="example-text">
<pre class="source">
# My Command #
--------------------

## 1. Usage ##
### 1.1 First Step ###
This is the first step to using this great command.

### 1.2 Second Step ###
This is the second step to using this command.
</pre>
<div class="rendered">
<h1>My Command</h1>
<hr/>
<h2>1. Usage</h2>
<h3>1.1 First Step</h3>
This is the first step to using this great command.<br>
<h3>1.2 Second Step</h3>
This is the second step to using this command.
</div>
</div>

## 2. Command Formatting <a name="cmdformatting"></a>

When rendering LaGriT commands, it is the convention on the LANL website to use **`bold monospaced font`** for literal keywords and `unstyled monospace` for variable names, mesh objects, etc.

This is difficult to do in pure Markdown, but fortunately very easy to do in HTML.
Since part of the Markdown specification includes embedded HTML parsing, LaGriT commands should be styled as:

<div class="markdown-example" id="example-text">
<pre class="source">
```
<b>addmesh / add</b> / mesh3 / mesh1 / mesh2 / [refine_factor] / [tet edge]
<b>addmesh / amr</b> / mesh3 / mesh1 / mesh2 /
<b>addmesh / append</b> / mesh3 / mesh1 / mesh2 /
```
</pre>
<div class="rendered">
<pre>
<b>addmesh / add</b> / mesh3 / mesh1 / mesh2 / [refine_factor] / [tet edge]
<b>addmesh / amr</b> / mesh3 / mesh1 / mesh2 /
<b>addmesh / append</b> / mesh3 / mesh1 / mesh2 /
</pre>
</div>
</div>

Note how the block in enclosed in the `<pre>` tags.

# Header 1 #

------------------------------------

This should be used for the title of the page.

**Markdown representation:**

    # Header 1 #

## Header 2 ##

This should be used for subheadings.

**Markdown representation:**

    ## Header 2 ##

### Header 3 ###

Alternate subheadings.

**Markdown representation:**

    ### Header 3 ###

## Code Blocks ##

Here is an example code block:

    cmo/create/cmohex / / / hex
    quadxyz/5,7,5/0.,0.,0./1.,0.,0./1.5,0.5,2.0/.5,.2,2.5/
    -1.,1.5,0./2.0,0.,0.0/2.1,1.9,2.4/-0.2,1.8,2.3/
    createpts/brick/xyz/5,7,5/1,0,0/connect

**Markdown representation** (indent of four spaces to begin code block):

        cmo/create/cmohex / / / hex
        quadxyz/5,7,5/0.,0.,0./1.,0.,0./1.5,0.5,2.0/.5,.2,2.5/
        -1.,1.5,0./2.0,0.,0.0/2.1,1.9,2.4/-0.2,1.8,2.3/
        createpts/brick/xyz/5,7,5/1,0,0/connect

## Bold/Italic Commands ##

By default, any code block will automatically render the Markdown bold syntax (`**bold text**`) as literal `**` within a code block.

To then bold/italic certain parts of a command, the best option is to use HTML:

**Output:**

<pre>
<b>cmo/create/</b>cmohex / / / hex
</pre>

**Markdown representation:**

	<pre>
	<b>cmo/create/</b>cmohex / / / hex
	</pre>


## Inline Code ##

Sections(s) of code within a larger normally typed paragraph, as in this example from ADDMESH:

Similar to merge except `imt`, `icr`, `itetclr` of *mesh2* have the value `max(imt(mesh1))` added to *mesh2*.

**Markdown representation:**

    Similar to merge except `imt`, `icr`, `itetclr` of *mesh2* have the value `max(imt(mesh1))` added to *mesh2*.

## Metadata ##

This is parsed by GitHub Pages and should be at the very top of each document.

	---
	title: "Example LaGriT Page"
	---


## Lists ##

### 1. Bullet Lists ###

**Output:**

* Bullet #1
* Bullet #2
* ...
* Bullet #N

**Markdown representation:**

	* Bullet #1
	* Bullet #2
	* ...
	* Bullet #N

### 2. Numbered Lists ###

**Output:**

1. Item #1
2. Item #2
3. ...
4. Item #N

**Markdown representation:**

	1. Item #1
	2. Item #2
	3. ...
	4. Item #N

## Quote Blocks ##

**Output:**

> I shall be telling this with a sigh
> 
> Somewhere ages and ages hence:
> 
> Two roads diverged in a wood, and I—
> 
> I took the one less traveled by,
> 
> And that has made all the difference.


**Markdown representation:**

	> I shall be telling this with a sigh
	> 
	> Somewhere ages and ages hence:
	> 
	> Two roads diverged in a wood, and I—
	> 
	> I took the one less traveled by,
	> 
	> And that has made all the difference.
