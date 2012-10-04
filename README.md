# vtools

### Description
vtools makes my research job easier by making the use of Vasp and Unix less enervating.

### How to use
- Read [this](http://xach.livejournal.com/278047.html?thread=674335) and get your system set up. Make sure you grab [this](https://github.com/quicklisp/quicklisp-slime-helper). Then load the system like so:
``` cl
CL-USER> (ql:quickload "vtools")
```
- Put the `get-enegies.sh` script in your path, then change the special variable `*get-energies-script*` appropriately.

- You'll usually want to be lazy and just summarize the energies. If `regex` is a regex that uniquely identifies the directories you want to summarize, then you'd summarize the energies like so:
``` cl
CL-USER> (vtools:summarize "regex")
```
Your data will get summarized into `*summary-directory*`. There is a default set, but you can change it if you want.

Assuming you have a Git remote set (the function currently pushes `master` to `origin`) in your `*summary-directory*`, and your public GPG key is authorized on the remote, you can summarize and push in one command with
``` cl
CL-USER> (vtools:summarize-and-push "regex")
```

#### Submitting jobs
If you want to use Emacs for submitting jobs, you can paste the `submit-jobs` function in your `*scratch*` buffer, evaluate it, then call it. Note that you will lose control of Emacs until all of the jobs are finished. (Yes, I too wish Emacs had threads.) The function is fairly self-documented -- if you know Emacs, you'll be able to figure it out.

   
### Todo
- add documentation
- make perspicuous and extensible API
- macro that takes a list of transforms
    - a transform has 2 parts -- a name (eg "hcp") and a transform (energy --> .25 * energy)
    - it constructs functions and applies the transforms to a given energy list
- with-grace-redraw macro

### Copying
See LICENSE file.



