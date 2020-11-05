# Using Git in your LaTeX document

In many of the document we author there is a need for proper configuration management. Hence we also use _GitLab_. The configuration settings explained here will allow you to show the status of your document (like the revision, last author to commit changes, etc) in the document itself.

Showing the Git status in a LaTeX document is actually simpler in the long run than when using `subversion`, or `svn` for short. The instructions here is based on the LaTeX package [`gitinfo2`](https://www.ctan.org/pkg/gitinfo2), which is standard with most LaTeX distributions live MacTeX, TeXLive and MikTeX.

To start off with, I recommend you add the `draft` option to your document class. For example
```latex
\documentclass[draft,a4paper,11pt]{article}
```
Next you need to call the required package, with its associated options. You can change these based on the `gitinfo2` documentation, but here is an example. I also include a command that changes the behaviour of how the watermark is added to the document.
```latex
\usepackage[markifdraft]{gitinfo2}
    \renewcommand{\gitMark}{Branch:\,\gitBranch\,@\,\gitAbbrevHash{}; Author:\,\gitAuthorName; Date:\,\gitAuthorIsoDate~\textbullet{}}
```
That was the really simple part. The next bit is slightly more tricky. You need to setup the necessary files on your machine so that the parameters required in the command above can be automatically populated. There are three parts to this:

1. Copy three files from the `git` folder to a hidden `.git/hooks/` folder in the root folder of your repository.
2. Make these files executable.
3. Initiate the `gitHeadInfo.gin` file.

In the root folder that you've checked out there should be a `.git/` folder which, in turn, contains a `hooks/` folder.  Copy the three files we include here, namely `post-checkout`, `post-commit` and `post-merge` into the `.git/hooks/` folder. (**Remember:** the `.git/` is a hidden folder, so you won't see it if you just open the root folder on your machine. So to copy those three files you will need to use the terminal or command line and the `cp` command.) 

Now an important step, check the file permissions and **make sure those three files are executable**. On a Mac and Linux machine you can achieve that (on the terminal) with the command
```shell
cd .git/hooks/
chmod ugo+x post-checkout
chmod ugo+x post-commit
chmod ugo+x post-merge
```

Next initiate the `gitHeadInfo.gin` file by running the following command in your root folder:
```shell
git checkout
```

If you now look what is inside the `.git/` folder (using `ls -lah .git` in terminal) you should see the `gitHeadInfo.gin` file.


You should now be able to compile your LaTeX document, and should see a nice watermark indicating the status of your document.
Once you remove the `draft` option, the watermark will disappear. 
If you want to retain the watermark in the final, non-draft version, change the package option to
```latex
\usepackage[mark]{gitinfo2}
```
**Disclaimer:** Be aware that these instructions are but one way to enable the watermark. Depending on how you clone your projects or the specifics of your machine, it may not work for you in exactly this way. We then refer you to the `gitinfo2` documentation that shipped
with your LaTeX distribution or Google to help you figure it out. 