package main

import (
	"bufio"
    "fmt"
    "os"
	"strconv"
	"strings"
)

type Transporter interface {
	addLoad(Trip) error
	print()
}

type Trip struct {
	destination string;
	weight float32
	deadline int
}

type Vehicle struct {
	vehicle 	string
	name 		string
	destination string
	speed 		float32
	capacity 	float32
	load 		float32	
}

type Truck struct {
	Vehicle
}

type Pickup struct {
	Vehicle
	isPrivate bool
}

type TrainCar struct {
	Vehicle
	railway string
}

func NewTruck(name string) Truck {
	if len(name) <= 0 {
		name = "Truck"
	}
	return Truck{
		Vehicle{vehicle: "Truck",
				name: name,
				destination: "",
				speed: 40,
				capacity: 10,
				load: 0},
	}
}

func NewPickup(name string) Pickup {
	if len(name) <= 0 {
		name = "Pickup"
	}
	return Pickup{
		Vehicle{vehicle: "Pickup",
				name: name,
				destination: "",
				speed: 60,
				capacity: 2,
				load: 0},
		true,
	}
}

func NewTrainCar(name string) TrainCar {
	if len(name) <= 0 {
		name = "TrainCar"
	}
	return TrainCar{
		Vehicle{vehicle: "TrainCar",
				name: name,
				destination: "",
				speed: 30,
				capacity: 30,
				load: 0},
		"CNR",
	}
}

func NewTorontoTrip(weight float32, deadline int) *Trip {
	return &Trip{"Toronto", weight, deadline}
}

func NewMontrealTrip(weight float32, deadline int) *Trip {
	return &Trip{"Montreal", weight, deadline}
}

func (t *Truck) addLoad(trip Trip) error {
	// Check if vehicle can make trip.
	if t.destination != "" && t.destination != trip.destination {
		return fmt.Errorf("Error: Other destination")
	}
	if t.capacity < (t.load + trip.weight) {
		return fmt.Errorf("Error: Out of capacity")
	}
	if t.destination == "" {
		// Add new destination to truck.
		t.destination = trip.destination
	}
	// Check if enough time to make trip
	distance := 400 
	if t.destination == "Montreal" {
		distance = 200
	}
	time := float64(distance) / float64(t.speed)
	if time > float64(trip.deadline) {
		return fmt.Errorf("Error: Not enough time to meet deadline.")
	}
	
	t.load = t.load + trip.weight
	return nil
}

func (p *Pickup) addLoad(trip Trip) error {
	if p.capacity < (p.load + trip.weight) {
		return fmt.Errorf("Error: Out of capacity")
	}
	if p.destination == "" {
		// Add new destination to truck.
		p.destination = trip.destination
	} else {
		// Check if destinations match
		if p.destination != trip.destination {
			return fmt.Errorf("Error: Other destination")
		}
	}
	// Check if enough time to make trip
	distance := 400 
	if p.destination == "Montreal" {
		distance = 200
	}
	time := float64(distance) / float64(p.speed)
	if time > float64(trip.deadline) {
		return fmt.Errorf("Error: Not enough time to meet deadline.")
	}
	
	p.load = p.load + trip.weight
	return nil
}

func (tc *TrainCar) addLoad(trip Trip) error {
	if tc.capacity < (tc.load + trip.weight) {
		return fmt.Errorf("Error: Out of capacity")
	}
	if tc.destination == "" {
		// Add new destination to truck.
		tc.destination = trip.destination
	} else {
		// Check if destinations match
		if tc.destination != trip.destination {
			return fmt.Errorf("Error: Other destination")
		}
	}
	// Check if enough time to make trip
	distance := 400 
	if tc.destination == "Montreal" {
		distance = 200
	}
	time := float64(distance) / float64(tc.speed)
	if time > float64(trip.deadline) {
		return fmt.Errorf("Error: Not enough time to meet deadline.")
	}
	
	tc.load = tc.load + trip.weight
	return nil
}

func (t *Truck) print() {
	fmt.Println(t.name + " to " + t.destination + " with " + fmt.Sprintf("%f", t.load) + " tons.")
}

func (p *Pickup) print() {
	fmt.Println(p.name + " to " + p.destination + " with " + fmt.Sprintf("%f", p.load) + " tons (Private: " + strconv.FormatBool(p.isPrivate) + ")")
}

func (tc *TrainCar) print() {
	fmt.Println(tc.name + " to " + tc.destination + " with " + fmt.Sprintf("%f", tc.load) + " tons (" + tc.railway + ")")
}

func printVehicles(t []Truck, p []Pickup, tc []TrainCar) {
	for _, a := range t {
		a.print()
	}
	for _, a := range p {
		a.print()
	}
	for _, a := range tc {
		a.print()
	}
}

func getInput() (string, float32, int) {
	fmt.Print("Destination: (t)oronto, (m)ontreal, else exit? ")
	reader := bufio.NewReader(os.Stdin)
	destination, _ := reader.ReadString('\n')
	destination = strings.ToLower(string(destination[0]))
	
	// Check for exit.
	if destination == "q" || destination == "e" {
		return "quit", 0, 0
	}
	
	fmt.Print("Weight: ")
	var weight float32
	fmt.Scanf("%f\n", &weight)
	
	fmt.Print("Deadline (in hours): ")
	var deadline int
	fmt.Scanf("%d\n", &deadline)
	
	return destination, weight, deadline
}

func main() {
	trucks := [2]Truck{NewTruck("Truck A"), NewTruck("Truck B")}
	pickups := [3]Pickup{NewPickup("Pickup A"), NewPickup("Pickup B"), NewPickup("Pickup C")}
	trains := [1]TrainCar{NewTrainCar("TrainCar A")}
	//vehicles := []interface{}{trucks, pickups, trains}
	trips := make([]Trip, 0)
	
	for {
		// Create trip.
		destination, weight, deadline := getInput()
		if destination == "quit" {
			fmt.Println("Not going to TO or Montreal, bye!")
			break
		}
		
		var trip *Trip
		if destination == "m" {
			trip = NewMontrealTrip(weight, deadline)
		} else if destination == "t" {
			trip = NewTorontoTrip(weight, deadline)
		}
		
		assigned := false
		
		// Check trucks
		for i, _ := range trucks {
			error := trucks[i].addLoad(*trip)
			if error == nil {
				// Trip added successfully
				assigned = true
				break
			} else {
				fmt.Println(error)
			}
		}
		if assigned == false {
			// check pickups
			for i, _ := range pickups {
				error := pickups[i].addLoad(*trip)
				
				if error == nil {
					// Trip added successfully
					assigned = true
					break
				} else {
					fmt.Println(error)
				}
			}
			if assigned == false {
				// check trains
				for i, _ := range trains {
					error := trains[i].addLoad(*trip)
					
					if error == nil {
						// Trip added successfully
						assigned = true
						break
					} else {
						fmt.Println(error)
					}
				}
			}
		}
		if assigned ==  true {
			// Save trip.
			trips = append(trips, *trip)
		}
	}
	
	// Print trips.
	fmt.Print("Trips: ")
	fmt.Println(trips)
	fmt.Println("Vehicles: ")
	printVehicles(trucks[:], pickups[:], trains[:])
}
