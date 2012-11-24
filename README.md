HaskDeep
========

What is it?
-----------
Command line tool that computes file hashes traversing recursively through
directory structure.
Known hashes are saved to file and they can be used to verify the original
files or a copy of them.

Quick start
-----------
Excute haskdeep without arguments and it will show you the help text:

    user@host:~$ haskdeep
    haskdeep - file hashing and audit

    Usage: haskdeep [-a|--audit] (-c|--computation ALGORITHM) (-r|--root DIRNAME) (-k|--known FILENAME)
      Computes hashes and audit a set of files

    Available options:
      -a,--audit               Audit
      -c,--computation ALGORITHM Computation mode: md5, sha1, sha256, skein512
      -r,--root DIRNAME        Root directory
      -k,--known FILENAME      Known hashes file
      -h,--help                Show this help text

Default usage:

1. create known hashes of files contained in a root directory (traversed recursively)

        user@host:~$ haskdeep -c md5 -r myimportantfiles/ -k knownhashes.txt

2. verify a copy of the same files comparing them with known hashes

        user@host:~$ haskdeep -a -c md5 -r copyofmyimportantfiles/ -k knownhashes.txt

Licensing
---------
Please see the file called LICENSE.

Contacts
--------
For question and comments:

- [MauroTaraborelli@gmail.com](mailto:MauroTaraborelli@gmail.com)
