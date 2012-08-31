# vtools

### Description
vtools makes my research job easier by making the use of Fortran and Unix less enervating.

### How to use
- You'll usually want to be lazy and just summarize the energies. Call it like so:

<pre><code>CL-USER> (vtools:summarize-energies *vasp-root-dir* "dir-regex")</code></pre>


### Todo
- add documentation
- make perspicuous and extensible API
- macro that takes a list of transforms
    - a transform has 2 parts -- a name (eg "hcp") and a transform (energy --> .25 * energy)
    - it constructs functions and applies the transforms to a given energy list
- with-grace-redraw macro

### Copying
See LICENSE file.



