HaskDeep
========

What is it?
-----------
Command line tool that computes file hashes traversing recursively through
a directory structure.
Known hashes are saved to file and they can be used to verify the original
files or a copy of them.

Quick start
-----------
Excute haskdeep without arguments and it will show you the help text:

    user@host:~$ haskdeep

    Usage: haskdeep COMMAND [-c|--computation MODE] [-r|--root DIRNAME] [-k|--known FILENAME] [-i|--ignore RULE]
      Computes hashes and audit a set of files

    Available options:
      -c,--computation MODE    md5 | sha1 | sha256 | skein512 - default md5
      -r,--root DIRNAME        root directory - default current directory
      -k,--known FILENAME      known hashes file - default known.haskdeep
      -i,--ignore RULE         regex to ignore files or directories
      -h,--help                Show this help text

    Available commands:
      compute                  Compute
      audit                    Audit

Default usage:

1. create known hashes of files contained in a root directory (traversed recursively)

        user@host:~$ haskdeep compute -c md5 -r myimportantfiles/ -k knownhashes.txt

2. verify a copy of the same files comparing them with known hashes

        user@host:~$ haskdeep audit -c md5 -r copyofmyimportantfiles/ -k knownhashes.txt

Licensing
---------
Please see the file called LICENSE.

Contacts
--------
For question and comments:

- [MauroTaraborelli@gmail.com](mailto:MauroTaraborelli@gmail.com)
