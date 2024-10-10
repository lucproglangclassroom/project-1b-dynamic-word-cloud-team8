[![Open in Codespaces](https://classroom.github.com/assets/launch-codespace-2972f46106e565e64193e422d61a12cf1da4916b45550586e14ef0a7c637dd04.svg)](https://classroom.github.com/open-in-codespaces?assignment_repo_id=16053571)
# Extra Credit Tracker
- Completed: Read from a file an "ignore list" of words to be ignored regardless of length.
- Completed: Treat words case-insensitively, i.e., ignore capitalization and the like
- Completed: Add an command-line argument for updating the word cloud only every k steps.
- Completed: Add an command-line argument for a minimum frequency to include a word in the word cloud.

# hello-scalatest-scala

Small project to get started with Scala and ScalaTest.


## Running the main program

```
$ sbt run 
```


## Running the tests

```
$ sbt test
```

or

```
$ sbt coverage test coverageReport
```

to see the code coverage percentages of your test suite.


## Running successive tasks with sbt

To speed up the edit-compile-test/run cycle, you can start sbt without arguments

```
$ sbt
```

and repeatedly run individual tasks

```
sbt> run
```

```
sbt> test
```

To exit sbt, enter

```
sbt> exit
```


## Running outside of sbt

This lets you use your application on the command-line.

First, create the startup script:

```
sbt stage
```

Then run outside of sbt like this:

```
./target/universal/stage/bin/hello-scalatest-scala
```

On Windows, you might need backslashes. WSL (Windows Subsystem for Linux) recommended instead.
