# parksinthepandemic

Repository for project looking at park use changes during the COVID-19 pandemic.

# Setup

First of all, clone this repository to your computer with the command below (make sure you've installed git):

To do this, open the command prompt and change to the directory where you would like to save the project. You can navigate using the `cd` command like so:

```
cd Documents\
```

Now you can clone the project to this location with the `clone` command: 

```bash
git clone https://github.com/AnnaPhD/parksinthepandemic.git
```

## Branches

Before we get into using git, let's talk about branches. 

Branches are essentially a unique set of changes that you make to the code. This means that you can keep different sets of changes separate from each other, then merge them together later down the line.  In this project, there are two branches that will always exist, `master` and `develop`, and any number of other branches for the development of new features. 

### Master branch

The `master` branch can be considered the live version. Ideally, this should be stable - it should run.

The master should only ever be merged with the `develop` branch.  

### Develop branch

The `develop` branch contains all the latest code. 

All users can merge their working features into the `develop` branch. 

### Feature branches

Use feature branches to write your code an add new features - you can have any number of these. 

You should only merge your feature branches into the `develop` branch. 

# Git Workflow

A git workflow is the plan used to describe how a project team will use git. This is the work flow I suggest. 

![](https://github.com/AnnaPhD/parksinthepandemic/tree/master/docs/figures/GitWorlflow.png)

#### Initiate a new branch

Use the command below to create a new branch for a feature you want to create: 

```bash
git checkout -b feature-name
```

This will switch you to your new branch, so you can now get started on it. 

#### Make a commit

Commits make up the record of the changes made to a project. If necessary, you can therefore roll back to any previous commit at any point. 

Before you can commit a file, you must add it to the staging environment, you can do this with the `add` command. You can add specific files explicitly, or add all files by using a full stop, like so:

```bash
git add .
```

Then you can make your commit and attach an informative note, like so: 

```bash
git commit -m "useful note about the commit"
```

#### Merge into develop

When you're ready you can merge your new feature into the `develop` branch. To do this, firstly switch to the `develop` branch with the command below:

```bash
git checkout develop
```

Then, use the command below to merge your branch (where `feature-name` is the name of your branch) : 

```bash
git merge feature-name
```

#### Push changes 

Once you're ready to update the online repository, switch the the branch you want to update, like so:

```bash
git checkout branch-name
```

Then push this branch to the repository with the `push` command: 

```bash
git push
```

#### Pulling changes

When you'd like to update your local version with code from the online repository, you can use the `pull` command.

First, switch to the branch that you'd like to update with the `checkout` command, and then use:

```bash
git pull
```

#### Delete a branch

When you've merged a branch and/or you no longer need it, you can delete it with the command below:

```bash
git branch -d branch-name
```

## 
