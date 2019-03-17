package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sync"
	"strings"
	"strconv"
)

type Path struct {
	path []Cell
	mc int
}

type Cell struct {
	i int
	j int
	cost int
	amount int
}

type Graph struct {
	header []string
	supply []int
	demand []int
	cells [][]Cell
}

var g Graph
var wgSolve sync.WaitGroup

func readFiles(fname string) [][]string {
	// Error check
	file, err := os.Open(fname)
	if err != nil {
		log.Fatalf("failed opening file: %s %s", err)
	}
	defer file.Close()
	
	// Read Description File
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)
	var lines []string
	var data [][]string
	for scanner.Scan() {
		lines = append(lines, strings.TrimSpace(scanner.Text()))
		data = append(data, strings.Split(lines[len(lines)-1], " "))
	}
	return data
}

func writeSolution() {
	// Create solution file.
	file, err := os.Create("solution.txt")
    if err != nil {
        log.Fatal("Cannot create file", err)
    }
    defer file.Close()
	// Write header
	for _,s := range g.header {
		fmt.Fprintf(file, s+"\t")
	}
	// Write supply and amounts
	fmt.Fprintf(file, "\n")
	for i, r := range g.cells {
		index := strconv.Itoa(i+1)
		fmt.Fprintf(file, "Source"+index+"\t")
		for _, c := range r { 
			s := strconv.Itoa(c.amount)
			fmt.Fprintf(file, s+"\t")
		}
		supply := strconv.Itoa(g.supply[i])
		fmt.Fprintf(file, supply+"\n")
	}
	fmt.Fprintf(file, "DEMAND\t")
	// Write demand.
	for _, d := range g.demand {
		demand := strconv.Itoa(d)
		fmt.Fprintf(file, demand+"\t")
	}
}

func extractData(desc [][]string, sol [][]string) {
	g.cells = make([][]Cell, len(sol) - 2)
	g.header = sol[0]
	
	// Extract supply.
	for i := 1; i < len(sol)-1; i++ {
		s, err := strconv.Atoi(sol[i][len(sol[i]) - 1])
		if err != nil {
			fmt.Println("Error in supply")
		}
		g.supply = append(g.supply, s)
	}
	
	// Extract Demand.
	l := len(sol[len(sol) - 1]) - 1
	if l == (len(sol[0]) - 1) {
		l -= 1
	}
	for i := 0; i < l; i++ {
		d, err := strconv.Atoi(sol[len(sol) - 1][i + 1])
		if err != nil {
			fmt.Println("Error in demand")
		}
		g.demand = append(g.demand, d)
	}
	
	// Extract Cost.
	for i := 0; i < (len(desc) - 2); i++ {
		for j := 0; j < (len(desc[i + 1]) - 2); j++ {		
			cost, err := strconv.Atoi(desc[i + 1][j + 1])
			if err != nil {
				fmt.Println("Error in cost")
			}
			num, err := strconv.Atoi(sol[i + 1][j + 1])
			if err != nil {
				if sol[i + 1][j + 1] != "-" {
					fmt.Println("Error in amount %s", err)
				}
			}
			
			c := Cell{i, j, cost, num}
			g.cells[i] = append(g.cells[i], c)
		}
	}
}

func emptyCells(cells [][]Cell) []Cell {
	var e []Cell
	for i := 0; i < len(cells); i++ {
		for j := 0; j < len(cells[i]); j++ {
			if cells[i][j].amount == 0 {
				e = append(e, cells[i][j])
			}
		}	
	}
	return e
}

func findNewPaths(e Cell) [][]Cell {
	var paths [][]Cell
	var nextPaths [][]Cell
	
	// Remove 1 from cell in row to balance supply.
	for j := 0; j < len(g.cells[e.i]); j++ {
		if j == e.j {
			continue
		}
		if g.cells[e.i][j].amount > 0 {
			// Create new path
			path := []Cell{e, g.cells[e.i][j]}
			paths = append(paths, path)
		}
	}
	// Add 1 in column to balance demand.
	for p := 0; p < len(paths); p++ {
		for i := 0; i < len(g.cells); i++ {
			if i == e.i {
				continue
			}
			cell := paths[p][1]
			if g.cells[i][cell.j].amount > 0 {
				// Create new path
				newPath := append(paths[p], g.cells[i][cell.j])
				nextPaths = append(nextPaths, newPath)
			}
		}
	}
	// Remove 1 in start column to balance supply and demand.
	l := len(nextPaths)
	for p := 0; p < l; p++ {	
		//fmt.Println("p: \t l:", p, l)
		//fmt.Println("Checking path: ", nextPaths[p])
		
		// Remove paths with incomplete cycles.
		if len(nextPaths[p]) < 3 {
			nextPaths = append(nextPaths[:p], nextPaths[p+1:]...)
			l--
			p--
			continue
		}
		cell := nextPaths[p][2]
		//fmt.Println("Checking last cell: ", g.cells[cell.i][e.j].amount)
		
		if g.cells[cell.i][e.j].amount > 0 {
			nextPaths[p] = append(nextPaths[p], g.cells[cell.i][e.j])
		} else {
			// Remove paths with incomplete cycles.
			nextPaths = append(nextPaths[:p], nextPaths[p+1:]...)
			l--
			p--
		}
	}
	return nextPaths
}

func marginalCost(cell Cell, result chan Path) {
	wgSolve.Add(1)
	// Find all possible new paths.
	paths := findNewPaths(cell)
	var optimalPath Path
	// Find marginal cost.
	var minCost int
	for p := 0; p < len(paths); p++ { 
		mc := 0
		// Sum up the cost
		//fmt.Println("P: ", paths[p])
		mc += paths[p][0].cost - paths[p][1].cost + paths[p][2].cost - paths[p][3].cost
		//fmt.Println("MC: ", mc)
		
		// Check if cost is better
		if (mc < minCost) {
			// Better solution found!
			minCost = mc;
			optimalPath = Path{paths[p], mc}
		}
	}
	result <- optimalPath
	wgSolve.Done()
}

func min(a, b int) int {
    if a < b {
        return a
    }
    return b
}

func reroute(p Path) {
	// Find max item amount to reroute
	num := min(p.path[1].amount, p.path[3].amount)
	negate := []int{1, -1}
	for i, c := range p.path {
		g.cells[c.i][c.j].amount += num * negate[i%2]
	}
}

func main() {
	// Get filename input
	descName := "4x4.txt"
	solName := "4x4_init.txt"
	fmt.Print("Please enter the description filename: ")
	fmt.Scanf("%s\n", &descName)
	fmt.Print("Please enter the initial solution filename: ")
	fmt.Scanf("%s\n", &solName)
	
	// Read files
	desc := readFiles(descName)
	sol := readFiles(solName)
	
	// Extract data
	extractData(desc, sol)
	fmt.Println("Supply: ", g.supply)
	fmt.Println("Demand: ", g.demand)
	fmt.Println(g.cells)
	
	// Find all empty cells
	empty := emptyCells(g.cells)
	fmt.Println("\n\nEmpty: ", empty)
	
	// Find optimal path
	pathChan := make(chan Path)
	var optimalPath Path
	for _, e := range empty {
		// Find optimal empty cell path
		go marginalCost(e, pathChan)
		p := <- pathChan
		// Check if optimal for all paths
		if p.mc < optimalPath.mc {
			optimalPath = p
		}
		//fmt.Println()
		//fmt.Println("Path:", p)
		//fmt.Println("Optimal:", optimalPath)
	}
	wgSolve.Wait()
	
	fmt.Println("Final Optimal:", optimalPath)
	reroute(optimalPath)
	writeSolution()
}