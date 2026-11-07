
# Conventions

Following in Section II.b is a alphabetic list of the LaGriT commands.
Conventions that apply to all commands include:

- **Line length.** A command line can be up to 1024 characters long.
  Lines longer than 80 characters are used in full, with a NOTE in the
  output, because LaGriT V3.3.4 and earlier read only the first 80
  characters of a line and ignored the rest without a message. A line
  longer than 1024 characters is an ERROR: its command is not run (it is
  never cut short or run in part), and LaGriT goes on with the next
  command. Comment lines can be any length.
- **Continuation lines.** A command is continued on the next line by an
  "&" as the last character of a line. A command joined from its
  continuation lines can be up to 16384 characters long; a longer one is
  an ERROR and is skipped, with all its continuation lines. A token cannot
  be split between two lines.
- **Token length.** A token (a name, keyword or value between delimiters)
  is used up to 32 characters. A longer word is cut to its first 32
  characters with a WARNING; names of mesh objects, attributes and files
  given to a command are therefore limited to 32 characters. A number
  is read in full.
- **File names, no paths.** LaGriT runs in one folder: a file is given
  by its name, without a path, since a command is split into words at
  each "/". `read / avs / ../dir/file.inp / mo` gives read the words
  `..`, `dir`, `file.inp` and `mo`. read checks for this: a file name
  that is a folder, or more words than the format takes (avs 7,
  lagrit 5, gmv 5, gocad 4), is an ERROR and nothing is read. Copy or
  link the file into the working folder and give its name.
- Messages about a line give its line number and file, for example
  `ERROR: line 12 of input.lgi is too long:`.

- Delimiters are comma, slash, equal sign, or blank. (',' '/', '=', ' ').

  Blanks on either side of other delimiters are ignored. Leading blanks
  are ignored. Commas are usually used for parameters that belong to the
  same logical set such as first point, last point, stride. Slashes are
  usually used to separate sets of parameters.

- The three parameters: first point, last point, stride can have
integer values which refer to actual sequential point numbers or they
can have the character- string values:

  **[pset](commands/PSET.md)**, **get**, name where name has been
  defined by a previous **[pset](commands/PSET.md)** command.

  The triplet: `1, 0, 0` refers to all points.

  The triplet: `0, 0, 0` refers to the set of points defined in the last
  geometry command.

- Commands should be typed in lower case, however names are case
sensitive.

  In the command description that follows certain symbols have special
  meaning.

  - `[ ]`          surround optional parameters

  - `,` or `/`      separates parameters

  - \_\_\_        underline specifies default

  - **bold** is used for literal keywords such as **xyz**.

- Comments are identified by \# or \* in the first column. Comments are
parsed; avoid using special characters especially 
`&` in comments.

- All names (`surface`, `region`, `pset`, ...) should be limited to 32
characters.

- The right hand rule is used to determine normals to planes and to
sheet surfaces. The first two points determin the first vector and the
first and third point determine the second vector. By curling the
fingers of the right hand from the first vector toward the second
vector, the right thumb will point in the direction of the normal.

- To separate commands on the same line use a semicolon (`;`).

- Three coordinate systems are used.

  **xyz** refers to the standard Cartesian coordinate system

  **rtz** refers to a cylindrical coordinate system aligned along the
  z-axis, where **r** is the radius measured from the zaxis, **t** (theta)
  is the angle measured in the xy-plane from the positive x-axis toward
  the positive y-axis and **z** is the height measured from the xy-plane.

  **rtp** refers to a spherical coordinate system, where **r** is the
  radius measured from the origin, **p** (phi) is the angle in the
  xy-plane measured from the positive x-axis toward the positive y-axis,
  **t** (theta) is the angle measured from the positive z-axis to the
  positive y axis.
