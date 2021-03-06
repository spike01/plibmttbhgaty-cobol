# PLIHBMTTBHGATY COBOL

Some COBOL written for PLIBMTTBHGATY v2.0 in London.

See [Denise's writeup](https://github.com/deniseyu/learning-cobol) for a much
more thorough
explanation of what we learned that day.

## Compiling

Fire up a useful Docker container:

```
docker run -it -v $PWD:/root/cobol -w /root/docker gregcoleman/docker-cobol /bin/bash
```

This volume maps wherever you've cloned this to `/root/cobol` within the
running Docker container.

In the container, head to the right directory:

```
cd /root/cobol
```

Compile:

```
cobc -x tictactoe.cob -o tictactoe
```

Run:
```
./tictactoe
```

Let's party like it's 1959
