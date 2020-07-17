package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
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

func checkUseless(prog []Decl) {
	leftSides := make(map[string]bool)
	used := make(map[string]bool)
	for _, decl := range prog {
		if _, ok := leftSides[decl.lhs]; ok {
			panic("double decl")
		}
		leftSides[decl.lhs] = true
		for _, tok := range decl.rhs {
			used[tok] = true
		}
	}

	used["galaxy"] = true
	for lhs, _ := range leftSides {
		if _, ok := used[lhs]; !ok {
			panic("unused decl:" + lhs)
		}
	}
}

func isInt(s string) bool {
	_, err := strconv.Atoi(s)
	return err == nil
}

func replaceConstants(prog []Decl) (resProg []Decl) {
	vals := make(map[string]string)

	for _, decl := range prog {
		if len(decl.rhs) != 1 {
			resProg = append(resProg, decl)
			continue
		}
		x := decl.rhs[0]
		if isInt(x) {
			log.Println("found constant: " + decl.lhs + " = " + x)
			vals[decl.lhs] = x
		} else {
			resProg = append(resProg, decl)
		}
	}

	for i, decl := range resProg {
		for j, tok := range decl.rhs {
			if val, ok := vals[tok]; ok {
				resProg[i].rhs[j] = val
			}
		}
	}

	return
}

func replaceIntegerNegations(prog []Decl) (resProg []Decl) {
	for _, decl := range prog {
		var newRhs []string
		for j := 0; j < len(decl.rhs); {
			if j+2 < len(decl.rhs) && decl.rhs[j] == "ap" && decl.rhs[j+1] == "neg" && isInt(decl.rhs[j+2]) {
				log.Println("negating " + decl.rhs[j+2])
				newRhs = append(newRhs, "-"+decl.rhs[j+2])
				j += 3
			} else {
				newRhs = append(newRhs, decl.rhs[j])
				j += 1
			}
		}
		resProg = append(resProg, Decl{lhs: decl.lhs, rhs: newRhs})
	}
	return
}

func main() {
	log.SetFlags(0)

	prog := readProgram("./galaxy.txt")

	checkUseless(prog)
	prog = replaceConstants(prog)
	prog = replaceIntegerNegations(prog)

	writeProgram(prog, os.Stdout)
}
