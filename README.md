# Minimal Triangle Path

Command-line tool that reads a number triangle from standard input and prints the minimal top-to-bottom path in the required format (`Minimal path is: …`).

## Running

```
sbt run < data_small.txt
```

You can also provide inline data:

```
cat <<'TRI' | sbt run
7
6 3
3 8 5
11 2 10 9
TRI
```

Output:

```
Minimal path is: 7 + 6 + 3 + 2 = 18
```

## Tests

```
sbt test
```
