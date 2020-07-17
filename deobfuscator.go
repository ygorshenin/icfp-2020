package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
)

type Decl struct {
	lhs string
	rhs []string
}

func tokenize(s string) []string {
	scanner := bufio.NewScanner(strings.NewReader(s))
	scanner.Split(bufio.ScanWords)
	var res []string
	for scanner.Scan() {
		res = append(res, scanner.Text())
	}
	return res
}

func readProgram(path string) (decls []Decl) {
	galaxyFile, err := os.Open(path)
	if err != nil {
		panic(err)
	}
	scanner := bufio.NewScanner(galaxyFile)
	for scanner.Scan() {
		s := scanner.Text()
		toks := tokenize(s)
		if toks[1] != "=" {
			panic("bad decl")
		}
		if toks[0][0] != ':' && toks[0] != "galaxy" {
			panic("bad lhs:" + s)
		}
		decls = append(decls, Decl{lhs: toks[0], rhs: toks[2:]})
	}
	if err := scanner.Err(); err != nil {
		panic(err)
	}
	return
}

func writeProgram(prog []Decl, out io.Writer) {
	for _, decl := range prog {
		fmt.Fprintf(out, "%s = %s\n", decl.lhs, strings.Join(decl.rhs, " "))
	}
}

func main() {
	prog := readProgram("./galaxy.txt")

	writeProgram(prog, os.Stdout)
}
