import java.io.*;
import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;

public class findOptimalTransport {
	
	public static void main(String[] args) throws Exception {
		if (args.length <= 0) 
			return;
			
		File file = new File(args[0]); 
		BufferedReader br = new BufferedReader(new FileReader(file)); 
		
		List<String[]> data = new ArrayList<String[]>();
		String line = ""; 
		while ((line = br.readLine()) != null) {
			String[] l = line.split("\\s");
			l = line.trim().split("\\s+");
			
			data.add(l);
		}
		
		// Note: Calling optimalSolution only will work, but won't print greedy solution info.
		// Find greedy solution.
		Graph g = new Graph(data);
		g.greedySolution();
		g.printSolution("Initial Greedy Solution");
		
		// Check for degenerate case.
		if (g.isDegenerate()) 
			System.out.println("\nWarning: Degenerate Case! Optimized solution may not be correct.");
		
		// Find optimized solution.
		List<int[]> route = g.optimalSolution();
		g.printSolution("Final Optimal Solution");
		
		// Print optimal route.
		if (route != null) {
			System.out.println("\n==============\nOptimal Route\n==============");
			g.printRoute(route);
		}
	}
}