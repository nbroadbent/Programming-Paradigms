type Point struct {
	x float64
	y float64
}

type Triangle struct {
	A Point
	B Point
	C Point
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