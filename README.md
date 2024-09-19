See: [blog post](https://photonsphere.org/post/2024-07-13-propellor/)

Export of Propellor distribution git via following command (initially and when updating -- take care to preserve the files of your configuration):

``` sh
git archive master | tar -x -C existingtargetdirectory
```
See explanation here: [Do a "git export" (like "svn export")?](https://stackoverflow.com/questions/160608/do-a-git-export-like-svn-export)
