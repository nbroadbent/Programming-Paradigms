package main

import (
	"fmt"
	"math"
	"math/rand"
	"sync"
)

type Point struct {
	x float64
	y float64
}

type Triangle struct {
	A Point
	B Point
	C Point
}

type Stack []Triangle
var wg sync.WaitGroup
var lock = sync.RWMutex{}

func (s *Stack) Push(t Triangle) {
	*s = append(*s, t)
}

func (s *Stack) Pop() Triangle {
	// Save top
	t := (*s)[len(*s)-1]
	// Remove top
	*s = (*s)[:len(*s)-1]
	return t
}

func SideLenght(p1 Point, p2 Point) float64 {
	return math.Sqrt(math.Pow((p1.x - p2.x), 2) + math.Pow((p1.y - p2.y), 2))
}

func (t Triangle) Perimeter() float64 {
	return SideLenght(t.A, t.B) + SideLenght(t.B, t.C) + SideLenght(t.A, t.C)
}

func (t Triangle) Area() float64 {
	return math.Abs((t.A.x*(t.B.y - t.C.y) + t.B.x*(t.C.y - t.A.y) + t.C.x*(t.A.y - t.B.y))/2)
}

func classifyTriangles(highRatio *Stack, lowRatio *Stack, ratioThreshold float64, 
						triangles []Triangle) {
	for _, t := range triangles {
		// Calculate ratio
		ratio := t.Perimeter() / t.Area()
		if ratio > ratioThreshold {
			lock.Lock()
			highRatio.Push(t)
			lock.Unlock()
		} else {
			lock.Lock()
			lowRatio.Push(t)
			lock.Unlock()
		}
	}
	defer wg.Done()
}


func triangles10000() (result [10000]Triangle) {
	rand.Seed(2120)
	for i := 0; i < 10000; i++ {
		result[i].A= Point{rand.Float64()*100.,rand.Float64()*100.}
		result[i].B= Point{rand.Float64()*100.,rand.Float64()*100.}
		result[i].C= Point{rand.Float64()*100.,rand.Float64()*100.}
	}
	return
}

func (t Triangle) print() {
	fmt.Println("Point A: " + fmt.Sprintf("%f", t.A))
	fmt.Println("Point B: " + fmt.Sprintf("%f", t.B))
	fmt.Println("Point C: " + fmt.Sprintf("%f", t.C))
}

func main() {
	triangles := triangles10000()
	var lowRatio Stack
	var highRatio Stack
	
	n := 1000
	length := 10000
	for i := 0; i < length; i += n {
		//fmt.Println(i)
		//fmt.Println(len(triangles[i:i+n]))
		wg.Add(1)
		go classifyTriangles(&highRatio, &lowRatio, 1.0, triangles[i:i+n])
	}
	wg.Wait()
	
	fmt.Println("==============\nLow Ratio\n==============")
	fmt.Println("Length: ", len(lowRatio))
	lowRatio.Pop().print()
	fmt.Println("\n==============\nHigh Ratio\n==============")
	fmt.Println("Length: ", len(highRatio))
	highRatio.Pop().print()
}