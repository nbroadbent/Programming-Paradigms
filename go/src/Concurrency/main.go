package main

import (
	"math/rand"
    "fmt"
	"time"
	"sync"
)

const (
	NumRoutines = 3
	NumRequests = 1000
)

// global semaphore monitoring the number of routines
var semRout = make(chan int, NumRoutines)
// global semaphore monitoring console
var semDisp = make(chan int, 1)

// Waitgroups to ensure that main does not exit until all done
var wgRout sync.WaitGroup
var wgDisp sync.WaitGroup

type Task struct {
	a, b float32
	disp chan float32
}

func solve(t *Task) {
	r := rand.Intn(15)
	time.Sleep(time.Duration(r) * time.Second)
	t.disp <- (t.a + t.b)
}

func handleReq(t *Task) {
	solve(t)
}

func ComputeServer()(chan *Task) {
	reqChan := make(chan *Task)
	go func() {
		defer close(reqChan) 
		for {
			req := <-reqChan
			semDisp <- 1 
			go handleReq(req)
			<- semRout
			defer wgRout.Done()
		}
		return
	}()
	return reqChan
}

func DisplayServer()(chan float32) {
	dispChan := make(chan float32)
	go func() {	
		for {
			res := <-dispChan // receive a new request
			fmt.Println("-------")
			fmt.Println("Result: " + fmt.Sprintf("%f", res))
			fmt.Println("-------")
			<- semDisp
			defer wgDisp.Done()
		}
		return
	}()
	return dispChan
}

func main() {
	dispChan := DisplayServer()
	reqChan := ComputeServer()
	
	go ComputeServer()
	go DisplayServer()
	
	for {
		var a, b float32
		// make sure to use semDisp
		fmt.Print("Enter two numbers: ")
		fmt.Scanf("%f %f \n", &a, &b)
		fmt.Printf("%f %f \n", a, b)
		if a == 0 && b == 0 {
			break
		}
		// Create task and send to ComputeServer
		semRout <-1
		reqChan <- &Task{a, b, dispChan}
		wgRout.Add(1) 
		wgDisp.Add(1)
		
		time.Sleep( 1e9 )
	}
	// Don’t exit until all is done
	wgDisp.Wait()
	wgRout.Wait()
	fmt.Println("main done")
}
