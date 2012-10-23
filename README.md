# vtools
Copyright (c) Andrew Mohrland <amohrland@gmail.com>
### Description
vtools makes my research job easier by making the use of Vasp and Unix less enervating.

### How to use
- Read [this](http://xach.livejournal.com/278047.html?thread=674335) and get your system set up. Make sure you grab [this](https://github.com/quicklisp/quicklisp-slime-helper). Then load the system like so:
``` 
CL-USER> (ql:quickload "vtools")
```
- Put the `get-enegies.sh` script in your path, then change the special variable `*get-energies-script*` appropriately.

#### Summarizing VASP output
- You'll usually want to be lazy and just summarize the energies. If `regex` is a regex that uniquely identifies the directories you want to summarize, then you'd summarize the energies like so:
``` cl
CL-USER> (vtools:summarize "regex")
```
Your data will get summarized into `*summary-directory*`. There is a default set, but you can change it if you want.

- Assuming you have a Git remote set in your `*summary-directory*`, and your public GPG key is authorized on the remote, you can summarize and push in one command with
``` cl
CL-USER> (vtools:summarize-and-push "regex")
```
The default local and remote branches are `master` and `origin`, respectively. You can change them by re-binding `*local-branch*` and/or `*remote-branch*`.

#### Submitting jobs
The following works when you're on the same machine that is running the jobs. You might want to start an Emacs session in GNU Screen, since your jobs could take a while.

If you have the necessary files in a particular directory, you submit a single job with
``` cl
CL-USER> (vtools:submit-single-job <path>)
```
`<path>` can be either a pathstring or a CL pathname object.

It's more likely that you want to submit a series of similar jobs, however. For example, you might want to find cohesive energy as a function of lattice parameter. No prob. Simply run sometime like
``` cl
CL-USER> (vtools:dojobs
		:path "/dir/to/hold/job/dirs/"
		:low 3.15
		:high 3.75
		:step 0.001)
```
and then go take a nap.

You can also exclude and/or include certain lattice parameters with the `:excludes-list` and `:additionals-list` keyword args. The default KPOINTS is 11x11x11. Change it for a particular jobs with the `:kpoints-list` keyword arg.
   
### Misc notes
- Currently, `dojobs` treats its lists of jobs to do as sets, not sequences -- Don't expect the jobs to be submitted in any particular order!

### Todo
- Add exception handling
- Fix interface to plotting software

### Suggestions
I'd like them. Andrew Mohrland <amohrland@gmail.com>.

### Copying
See LICENSE file.



