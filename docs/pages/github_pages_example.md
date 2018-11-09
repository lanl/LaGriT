---
title: "Example LaGriT Page"
---

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
