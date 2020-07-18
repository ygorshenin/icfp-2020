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

type Ast struct {
	op     string
	term   string
	left   *Ast
	right  *Ast
	parent *Ast
}

func (ast *Ast) setLeft(child *Ast) {
	ast.left = child
	if child != nil {
		child.parent = ast
	}
}

func (ast *Ast) setRight(child *Ast) {
	ast.right = child
	if child != nil {
		child.parent = ast
	}
}

func ap(l, r *Ast) *Ast {
	f := &Ast{op: "ap"}
	f.setLeft(l)
	f.setRight(r)
	return f
}

func (ast *Ast) compilePointed(argnum *int) []string {
	if ast == nil {
		*argnum += 1
		return []string{"arg" + strconv.Itoa(*argnum-1)}
	}
	if ast.term != "" {
		return []string{ast.term}
	}
	if ast.op != "ap" {
		panic("")
	}
	var ret []string
	ret = append(ret, "ap")
	ret = append(ret, ast.left.compilePointed(argnum)...)
	ret = append(ret, ast.right.compilePointed(argnum)...)
	return ret
}

func (ast *Ast) compilePointless() []string {
	ret := ast.compilePointed(new(int))
	for len(ret) > 0 {
		if !strings.HasPrefix(ret[len(ret)-1], "arg") {
			break
		}
		ret = ret[0 : len(ret)-1]
	}
	return ret
}

func (ast *Ast) compile() []string {
	//	return ast.compilePointed(new(int))
	return ast.compilePointless()
}

func (ast *Ast) String() string {
	if ast == nil {
		return "DUMMY" // introduce argument positions if needed
	}
	if ast.term != "" {
		return ast.term
	}
	return ast.op + " " + ast.left.String() + " " + ast.right.String()
}

func toAst(toks []string) (ast *Ast, rest []string) {
	if len(toks) == 0 {
		return nil, toks
	}
	ast = new(Ast)
	if toks[0] == "ap" {
		ast.op, toks = toks[0], toks[1:]
		lc, toks := toAst(toks)
		rc, toks := toAst(toks)
		ast.setLeft(lc)
		ast.setRight(rc)
		rest = toks
	} else {
		ast.term = toks[0]
		rest = toks[1:]
	}
	return
}

func (ast *Ast) numNodes() int {
	if ast == nil {
		return 0
	}
	return 1 + ast.left.numNodes() + ast.right.numNodes()
}

func (ast *Ast) reduce() *Ast {
	if ast.numNodes() < 5 {
		return ast
	}

	if ast.left.numNodes() != 1 {
		ast.left = ast.left.reduce()
	}
	log.Println("need to eval ", ast.left.term)
	return ast
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
		if s[0] == '#' {
			continue
		}
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

func inlineConstants(prog []Decl) (resProg []Decl) {
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

func inlineIntegerNegations(prog []Decl) (resProg []Decl) {
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

func experiment(prog []Decl) {
	for _, decl := range prog {
		ast, rest := toAst(decl.rhs)
		if len(rest) != 0 {
			panic("bad ast")
		}
		_ = ast

		//		ast.reduce()
	}
}

func (ast *Ast) findNode(s string) *Ast {
	if ast == nil {
		return nil
	}
	if ast.term == s {
		return ast
	}
	if n := ast.left.findNode(s); n != nil {
		return n
	}
	if n := ast.right.findNode(s); n != nil {
		return n
	}
	return nil
}

func evaluateC(prog []Decl) (resProg []Decl) {
	for _, decl := range prog {
		ast, rest := toAst(decl.rhs)
		if len(rest) != 0 {
			panic("bad ast")
		}
		for {
			c := ast.findNode("c")
			if c == nil {
				break
			}

			log.Println("found")

			x := c.parent.right
			log.Println("x ok")
			y := x.parent.parent.right
			log.Println("y ok")
			z := y.parent.parent.right
			log.Println("z ok")
			pz := z.parent
			if x == nil || y == nil || z == nil {
				panic("")
			}
			log.Println(x, y, z)
			//			log.Println(x.term, y.term, z.term)

			f := ap(x, z)
			pz.setLeft(f)
			pz.setRight(y)
		}

		resProg = append(resProg, Decl{lhs: decl.lhs, rhs: ast.compile()})
	}
	return
}

func evaluateB(prog []Decl) (resProg []Decl) {
	for _, decl := range prog {
		ast, rest := toAst(decl.rhs)
		if len(rest) != 0 {
			panic("bad ast")
		}
		for {
			c := ast.findNode("b")
			if c == nil {
				break
			}

			log.Println("found")

			x := c.parent.right
			y := x.parent.parent.right
			z := y.parent.parent.right
			pz := z.parent
			if x == nil || y == nil || z == nil {
				panic("")
			}

			f := ap(y, z)
			pz.setLeft(x)
			pz.setRight(f)
		}

		resProg = append(resProg, Decl{lhs: decl.lhs, rhs: ast.compile()})
	}
	return
}

func evaluateK(prog []Decl) (resProg []Decl) {
	for _, decl := range prog {
		ast, rest := toAst(decl.rhs)
		if len(rest) != 0 {
			panic("bad ast")
		}
		for {
			c := ast.findNode("t") // K is t
			if c == nil {
				break
			}

			log.Println("found")

			x := c.parent.right
			y := x.parent.parent.right
			py := y.parent
			if x == nil || y == nil {
				panic("")
			}

			py.op, py.term, py.left, py.right = x.op, x.term, x.left, x.right
		}

		resProg = append(resProg, Decl{lhs: decl.lhs, rhs: ast.compile()})
	}
	return
}

func evaluateS(prog []Decl) (resProg []Decl) {
	for _, decl := range prog {
		ast, rest := toAst(decl.rhs)
		if len(rest) != 0 {
			panic("bad ast")
		}
		for {
			c := ast.findNode("s")
			if c == nil {
				break
			}

			log.Println("found")

			x := c.parent.right
			y := x.parent.parent.right
			z := y.parent.parent.right
			pz := z.parent
			if x == nil || y == nil || z == nil {
				panic("")
			}

			l := ap(x, z)
			r := ap(y, z)
			pz.setLeft(l)
			pz.setRight(r)
		}

		resProg = append(resProg, Decl{lhs: decl.lhs, rhs: ast.compile()})
	}
	return
}

func main() {
	log.SetFlags(0)

	//	prog := readProgram("./galaxy.txt")
	//	prog := readProgram("./t.txt")
	prog := readProgram("./small.txt")

	checkUseless(prog)
	prog = inlineConstants(prog)
	prog = inlineIntegerNegations(prog)
	prog = evaluateC(prog)
	prog = evaluateB(prog)
	prog = evaluateK(prog)
	prog = evaluateS(prog)

	//	experiment(prog)

	writeProgram(prog, os.Stdout)
}
