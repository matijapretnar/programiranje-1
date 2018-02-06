# Notes on publishing solutions

If you are reading this file, you must be on the `solutions` branch of the private FMF repo. As the name suggests, this branch contains solutions to exercises.

You must never merge this branch into `master`, because `master` should always contain unsolved exercises! Instead, if you want to reveal solutions to students, there is a branch called `solutions-$year` on the public github repo. Here's an example how to get a file from the `solutions` branch published:

    git checkout solutions-2017
    git merge master
    git checkout --patch solutions ap-1-zajem-podatkov/vaje/
    git commit -m 'Publish solutions for ap-1'
    git push matija solutions-2017  

Work on the solutions should always occur on the `solutions` branch and can then be published to the `solutions-$year` branch.
