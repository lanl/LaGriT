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
3. [Inline Code](#inlinecode)
4. [Linking to Other Pages](#linking)

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
  <p>This is the first step to using this great command.<br></p>
<h3>1.2 Second Step</h3>
  <p>This is the second step to using this command.</p>
</div>
</div>

## 2. Command Formatting <a name="cmdformatting"></a>

When rendering LaGriT commands, it is the convention on the LANL website to use **`bold monospaced font`** for literal keywords and `unstyled monospace` for variable names, mesh objects, etc.

This is difficult to do in pure Markdown, but fortunately very easy to do in HTML.
Since part of the Markdown specification includes embedded HTML parsing, LaGriT commands should be styled as:

<div class="markdown-example" id="example-text">
<pre class="source">

<!--
GH Pages keeps trying to render this as HTML instead of raw code.
So, we're going to have to escape the HTML tags to get them literally rendered...
-->

<code>

&lt;pre&gt;<br>
&lt;b&gt;addmesh / add&lt;/b&gt; / mesh3 / mesh1 / mesh2 / [refine_factor] / [tet edge]
&lt;b&gt;addmesh / amr&lt;/b&gt; / mesh3 / mesh1 / mesh2 /
&lt;b&gt;addmesh / append&lt;/b&gt; / mesh3 / mesh1 / mesh2 /
&lt;/pre&gt;

</code>
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

## 3. Inline Code <a name="inlinecode"></a>

Similarily to the section above, for sections(s) of code within a larger normally typed paragraph, literal keywords should be bold and all other objects should be unstyled. Mesh objects should be formatted with *`italic monospace`*.

<div class="markdown-example" id="example-text">
<pre class="source">
Similar to merge except `imt`, `icr`, `itetclr` of *`mesh2`* have the value `max(imt(mesh1))` added to *`mesh2`*.
</pre>
<div class="rendered">
<p>Similar to merge except <code>imt</code>, <code>icr</code>, <code>itetclr</code> of <i><code>mesh2</code></i> have the value <code>max(imt(mesh1))</code> added to <i><code>mesh2</code></i>.</p>
</div>
</div>

## 4. Linking to Other Pages <a name="linking"></a>

You can link to other pages on this site by either using the literal URL, or (more appropriately) with a relative link to the corresponding Markdown file.

<div class="markdown-example" id="example-text">
<pre class="source">
As an example, [this page](https://lanl.github.io/LaGriT/pages/docs/commands/ADDMESH.html)
will take you to the same place as [clicking here](docs/commands/ADDMESH.md).
</pre>
<div class="rendered">
<p>As an example, <a href="https://lanl.github.io/LaGriT/pages/docs/commands/ADDMESH.html">this page</a> will take you to the same place as <a href="https://lanl.github.io/LaGriT/pages/docs/commands/ADDMESH.html">clicking here</a>.</p>
</div>
</div>

