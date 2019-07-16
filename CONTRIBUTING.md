# Contributing Code

Contributor: A contributor is someone from the outside not on the core development team of the project that wants to contribute some changes to a project. Before a pull request can be accepted, a external contributor must submit a Contributor License Agreement (CLA) as described below. 

Collaborator: A collaborator is part of the LANL core development team and has commit access to the main repository.


# External Contributions to LaGriT

Before submitting a pull request, please make sure you have done the following:

1. Fork the repository and fork your working branch from master
2. Describe your test plan in your Pull Request
3. Ensure all test suites pass (`test/suite.py`)
4. Make sure you have completed the CLA

## Copyright Notice for Files

Copy and paste this to the top of any created source code (replacing `!` with any language-specific comment char):

```
!
! Copyright (c) 2017 Los Alamos National Laboratory, LLC
! All rights reserved.
!
! This source code is licensed under the BSD-style license found in the
! LICENSE.md file in the root directory of this source tree.
!
```

## Contributor License Agreement (CLA)

In order to accept your pull request, we need you to submit a CLA. If you are submitting a pull request for the first time, just let us know after you have completed the CLA so that we can cross-check signers with your GitHub username. This license is for your protection as a Contributor as well as the protection of LANL and the LaGriT users; it does not change your rights to use your own Contributions for any other purpose.

Sign the CLA here: https://www.clahub.com/agreements/lanl/LaGriT

# Instructions for Contributors and Collaborators

- anything in the master branch is always deployable
- identify any issues including bugs and feature requests, use appropriate title and reproducable description
- update your local repo, branch, or fork often to stay current with the master branch
- use fork for off-site contributors and large number of repo changes
- use branch for approved collaborators for specific code development to be saved and perhaps merged
- use local clone for admin level collaborators updating the master branch
- all code committed to LaGriT code must use or be compatible with our BSD-3 license 
- all code must be tested before it can be merged into the mainstream


## Large Files

Git is designed for source code, scripts, and smaller datasets and is repositories are not more than a few gigabytes.
Larger data files may occasionally need to accompany a repository, and if this is the case, consider these options:
- a shared project data directory on an HPC resource or local disk space
- include an empty directory that has a .gitignore file to ignore all of its contents and then share file transfer services
- if repo absolutely must contain larger data files for unit testing needs, use the Git Large File Storage (LFS) extension so large files are not versioned (only the latest version is kept). 

# Notes on Branches, Forks, and Clones

## Create a Branch
For collaborators such as students and visitors working on a specific feature or idea. This is good for code development that may or may not be merged with master, but allows you to keep the work with the repo.

```
LaGriT is my working directory from master branch
clone is current repo (for synching my work)

-------------------
Create a branch in my own space

  % git checkout -b tamiller_dev

Switched to a new branch 'tamiller_dev'

-------------------
Show what branches local git can see
  % git branch

  master
* tamiller_dev

-------------------
Make branch live on github repo (only local until this is done).
  git push --set-upstream origin tamiller_dev

-------------------
Switch to master and update from git repo
  % git checkout master

Switched to branch 'master'
Your branch is up-to-date with 'origin/master'.

-------------------
  % git pull

remote: Enumerating objects: 6, done.
remote: Counting objects: 100% (6/6), done.
remote: Total 6 (delta 5), reused 6 (delta 5), pack-reused 0
Unpacking objects: 100% (6/6), done.
From https://github.com/lanl/LaGriT
   c2477a8..42507ad  master     -> origin/master
Updating c2477a8..42507ad
Fast-forward
 test/level01/single_triangle/reference/logx3dgen | 0
 test/level01/single_triangle/reference/outx3dgen | 0
 2 files changed, 0 insertions(+), 0 deletions(-)
 create mode 100644 test/level01/single_triangle/reference/logx3dgen
 create mode 100644 test/level01/single_triangle/reference/outx3dgen

-------------------
Switch to branch and check status
  % git checkout tamiller_dev
  % git status

On branch tamiller_dev
Your branch is up-to-date with 'origin/tamiller_dev'.
nothing to commit, working directory clean


-------------------
Get difference between master and branch
  % git diff master..tamiller_dev

diff --git a/test/level01/single_triangle/reference/logx3dgen b/test/level01/single_triangle/reference/logx3dgen
deleted file mode 100644
index e69de29..0000000
diff --git a/test/level01/single_triangle/reference/outx3dgen b/test/level01/single_triangle/reference/outx3dgen
deleted file mode 100644
index e69de29..0000000
es04:LaGriT:bsh% git diff master..
diff --git a/test/level01/single_triangle/reference/logx3dgen b/test/level01/single_triangle/reference/logx3dgen
deleted file mode 100644
index e69de29..0000000
diff --git a/test/level01/single_triangle/reference/outx3dgen b/test/level01/single_triangle/reference/outx3dgen
deleted file mode 100644
index e69de29..0000000
```

If there commits but you want to see them, status does not show.
push so origin (github repo) is curent with your work directory.
Once push is complete the commits will show under your  branch.
You can now create a pull request (compare tamiller_dev to master)
```
illposed:src:bsh% git status
On branch tamiller_dev
Your branch is ahead of 'origin/tamiller_dev' by 5 commits.
  (use "git push" to publish your local commits)
nothing to commit, working directory clean

illposed:src:bsh% git push
To https://github.com/lanl/LaGriT.git
   d7fcef5..149f303  tamiller_dev -> tamiller_dev
```


## Fork a project
For off-site collaborators and changes involving many repo files.  This is very useful for editing a large number of files, such as git pages. This will keep all the notifications from emails and allow you to pull and merge into a single commit.

The following are instructions for updating your fork from the master repo.
```
Fork millerta/LaGriT-1

Use brower to Synch my fork with orginal so fork is current.

Open your fork of the repository.

Click the compare button under Clone button.

This will open a page titled Comparing Changes NOTE you have jumped to the upstream original version of the repository.
If you were to do a regular pull request then you would be bringing your changes into the original version.
But in this case we want to flip the direction and pull changes from the original version to our fork.

Change the base fork to your repository

You're now back to your fork but showing branches not forks.
Click on compare across forks to get back your base fork option.

Change the head fork to the upstream (original) repository

You'll see one of two options:

1. "There isn't anything to compare" so you don't have to do anything.

2. A list of commits.  Create a pull request
Note that this pull request is to you!
So you can confirm that it's ok and merge as necessary.
If there are any merge conflicts then it's up to you to figure out what's gone wrong and sort them out.

And now you're ready to continue working on your fork
```

## Working from a clone
Pull requests are approved only by the development team. 

This is a typical workflow for pushing code to be pulled to master branch.
```
git pull
git add -A
git commit -a
git push
```


