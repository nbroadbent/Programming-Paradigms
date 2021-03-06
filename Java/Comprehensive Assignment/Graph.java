import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

public class Graph {
	private String[] header;
	private int[] supply;
	private int[] demand;
	private int[][] cost;
	private int[][] supplied;
	private List<String[]> data;
	
	public Graph(List<String[]> data) {
		this.data = data;
		header = data.get(0);
		extractData(data);
		supplied = new int[data.size()-2][data.get(1).length - 2];
	}
	
	private void extractData(List<String[]> data) {
		// Find supply. Subtract 2: first row is label, last is demand.
		supply = new int[data.size()-2];
		for (int i = 1; i < data.size()-1; i++) {
			//System.out.println(data.get(i)[data.get(i).length - 1]);
			supply[i-1] = Integer.parseInt(data.get(i)[data.get(i).length - 1].replaceAll("\\s+",""));
		}
		
		// Find demand.
		int len = data.get(data.size() - 1).length - 1;
		if (len == data.get(0).length - 1) {
			// One cell in last row is the sum. Remove it.
			len--;
		}
		demand = new int[len];
		for (int i = 0; i < len; i++) {
			demand[i] = Integer.parseInt(data.get(data.size() - 1)[i + 1].replaceAll("\\s+",""));
		}
		
		// Find cost.
		cost = new int[data.size()-2][data.get(1).length - 2];
		for (int i = 0; i < data.size()-2; i++) {
			for (int j = 0; j < data.get(i+1).length - 2; j++) {
				cost[i][j] = Integer.parseInt(data.get(i+1)[j + 1].replaceAll("\\s+",""));
			}
		}
	}

	public void greedySolution() {
		int count = 0;
		while (!checkComplete()) {
			int[] result = findMinCost();
			supplyItems(result);
		}
		// Reinitialize supply and demand data.
		extractData(data);
	}
	
	public List<int[]> optimalSolution() {
		if (numOccupiedCells() == 0) 
			greedySolution();
		
		List<int[]> empty = new ArrayList<int[]>();
		for (int i = 0; i < supplied.length; i++) {
			for (int j = 0; j < supplied[i].length; j++) {
				if (supplied[i][j] == 0) {
					empty.add(new int[]{i, j});
				}
			} 
		}
		
		// Find better route.
		int minCost = 0;
		List<int[]> optimalRoute = new ArrayList<int[]>();;
		for (int i = 0; i < empty.size(); i++) {
			// Find all possibles new routes for empty node.
			List<List<int[]>> routes = findNewRoutes(empty.get(i));
			
			if (routes == null)
				continue;
			
			// Calculate marginal cost of each route.
			int mc = 0;
			int[] negate = new int[]{1, -1};
			for (int r = 0; r < routes.size(); r++) {
				for (int j = 0; j < routes.get(r).size(); j++) {
					// Alternate addition and subtraction of costs.
					int[] node = routes.get(r).get(j);
					mc += cost[node[0]][node[1]] * negate[j%2];
				}
							
				if (mc < minCost) {
					// Found a better route!
					minCost = mc;
					optimalRoute = new ArrayList<int[]>(routes.get(r));
				}
				mc = 0;
			}
		}
		
		if (optimalRoute.size() <= 0) {
			System.out.println("\nNo better solution found!");
			return null;
		}

		// System.out.println("\n==============\nOptimal Route\n==============");
		// for (int j = 0; j < optimalRoute.size(); j++) {
			// System.out.println("Node " + j + ": " + Arrays.toString(optimalRoute.get(j)));
		// }
		// System.out.println("\nMargin: " + minCost);
		
		// Transfer min(deductions) to prevent negatives
		int amount = minSupplied(optimalRoute.get(1), optimalRoute.get(3));
		int[] betterNode0 = optimalRoute.get(0);
		int[] lesserNode0 = optimalRoute.get(1);
		int[] betterNode1 = optimalRoute.get(2);
		int[] lesserNode1 = optimalRoute.get(3);
		int[] lastNode = optimalRoute.get(3);
		
		supplied[betterNode0[0]][betterNode0[1]] += amount;
		supplied[lesserNode0[0]][lesserNode0[1]] -= amount;
		supplied[betterNode1[0]][betterNode1[1]] += amount;
		supplied[lesserNode1[0]][lesserNode1[1]] -= amount;
		
		return optimalRoute;
	}
	
	public int calculateCost() {
		int total = 0;
		for (int i = 0; i < supplied.length; i++) {
			for (int j = 0; j < supplied[i].length; j++) {
				total += supplied[i][j] * cost[i][j];
			}
		}
		return total;
	}
	
	public boolean isDegenerate() {
		return (supply.length + demand.length -1) != numOccupiedCells();
	}
	
	public void printSolution(String title) {
		System.out.println("\n=======================\n"+title+"\n=======================");
		// Print header data.
		for (int i = 1; i < header.length; i++) {
			System.out.print("\t" + header[i]);
		}
		printSupplied();
		printDemand();
		//System.out.println(Arrays.toString(demand));
		System.out.println("\nTotal Cost is " + calculateCost());
	}
	
	public void printDemand() {
		System.out.print("\nDEMAND");
		for (int i = 0; i < demand.length; i++) {
			System.out.print("\t" + demand[i]);
		}
	}
	
	public void printSupplied() {
		// Print supplied data.
		for (int i = 0; i < supplied.length; i++) {
			System.out.print("\nSource" + (1+i));
			for (int j = 0; j < supplied[i].length; j++) {
				System.out.print("\t" + supplied[i][j]);
			}
			System.out.print("\t" + supply[i]);
		}
	}
	
	public void printRoute(List<int[]> route) {
		if (route == null) return;
		for (int i = 0; i < route.size(); i++) {
			System.out.print(route.get(i)[0] + "-" + header[route.get(i)[1]+1] + "\t");
		}			
	}
	
	private List<List<int[]>> findNewRoutes(int[] start) {
		List<int[]> route = new ArrayList<int[]>();		
		List<List<int[]>> routes = new ArrayList<List<int[]>>();
		
		// Add 1 to empty cell.
		route.add(start);
			
		// Remove 1 from cell in row to balance supply.
		int[] cell = new int[2];
		boolean removed = false;
		//System.out.println("Empty: " + start[0] + " " + start[1]);
		for (int i = 0; i < supplied[start[0]].length; i++) {
			if (i == start[1])
				continue;
			if (supplied[start[0]][i] > 0) {
				// Add new route to all routes
				cell = new int[]{start[0], i};
				route.add(cell);
				routes.add(new ArrayList<int[]>(route));
				route.remove(cell);
				removed = true;
				//break;
			}
		}
		if (!removed) {
			//System.out.println("Error: Supply not balanced!");
			return null;
		}
		
		// Add 1 in column to balance demand.
		removed = false;
		for (int r = 0; r < routes.size(); r++) {
			cell = routes.get(r).get(1);
			for (int i = 0; i < supplied.length; i++) {
				if (i == start[0])
					continue;
				if (supplied[i][cell[1]] > 0) {
					// Modify.
					cell = new int[]{i, cell[1]};
					routes.get(r).add(cell);
					route.remove(cell);
					removed = true;
					//break;
				}
			}
		}
		if (!removed) {
			//System.out.println("Error: Demand not balanced!");
			return null;
		}
		
		// Remove 1 in start column to balance supply and demand.
		for (int r = 0; r < routes.size(); r++) {
			// Skip routes with incomplete cycles.
			if (routes.get(r).size() < 3)
				continue;
			cell = routes.get(r).get(2);
			if (supplied[cell[0]][start[1]] > 0)  
				routes.get(r).add(new int[]{cell[0], start[1]});
			//else
			//	route = newRoute(start, new int[][]{{cell[0], start[1]}});
		}
		// Filter routes to keep only valid cycles.
		return filterRoutes(routes, 4);
	}
	
	private List<List<int[]>> filterRoutes(List<List<int[]>> routes, int n) {
		List<List<int[]>> r = new ArrayList<List<int[]>>();
		for (int i = 0; i < routes.size(); i++) {
			if (routes.get(i).size() == n) 
				r.add(routes.get(i));
		}
		return r;
	}
	/*private List<int[]> newRoute(int[] start, int[][] exclude) {
		List<int[]> route = new ArrayList<int[]>();		
		
		// Add 1 to empty cell.
		route.add(start);
		
		// Remove 1 from node in row to balance supply.
		int[] node = new int[2];
		boolean removed = false;
		//System.out.println("Empty: " + start[0] + " " + start[1]);
		for (int i = 0; i < supplied[start[0]].length; i++) {
			int[] n = new int[]{start[0], i};
			if (contains(n, exclude)) {
				System.out.println("CONTAINS " + n);
				continue;
			}
			if (supplied[n[0]][n[1]] > 0) {
				node = n;
				route.add(n);
				removed = true;
				break;
			}
		}
		if (!removed) {
			//System.out.println("Error: Supply not balanced!");
			return null;
		}
		
		System.out.println(Arrays.toString(node));
		
		// Add 1 in column to balance demand.
		removed = false;
		for (int i = 0; i < supplied.length; i++) {
			int[] n = new int[]{i, node[1]};
			if (contains(n, exclude)) {
				System.out.println("CONTAINS " + n);
				continue;
			}
			if (supplied[n[0]][n[1]] > 0) {
				node = n;
				route.add(n);
				removed = true;
				break;
			}
		}
		if (!removed) {
			//System.out.println("Error: Demand not balanced!");
			return null;
		}
		
		// Remove 1 in start column to balance supply and demand.
		if (supplied[node[0]][start[1]] > 0)  
			route.add(new int[]{node[0], start[1]});
		return route;
	}*/
	
	private boolean contains(int[] n, int[][] l) {
		for (int i = 0; i < l.length; i++) {
			if (n[0] == l[i][0] && n[1] == l[i][1])
				return true;
		}
		return false;
	}
	
	private int numOccupiedCells() {
		int count = 0;
		for (int i = 0; i < supplied.length; i++) {
			for (int j = 0; j < supplied[i].length; j++) {
				if (supplied[i][j] != 0) {
					count++;
				}
			} 
		}
		return count;
	}
	
	private void makeCycle(int[] start, int endIndex) {
		// Move right
		for (int i = start[0]; i < endIndex; i++) {
			if (i == start[0])
				continue;
			
		}
	}
	
	private void makeCycles(int[] start) {
		
	}
	
	private boolean checkComplete() {
		// Check if demand met.
		for (int i = 0; i < demand.length; i++) {
			if (demand[i] != 0) 
				return false;
		}
		return true;
	}
	
	private void supplyItems(int[] result) {
		int s = supply[result[1]];
		int d = demand[result[2]];
		
		if (s > d) {
			// Supply all requested.
			supplied[result[1]][result[2]] += d;
			s -= d;
			d = 0;
		} else {
			// Supply all available.
			supplied[result[1]][result[2]] += s;
			d -= s;
			s = 0;
		}
		// Update data.
		supply[result[1]] = s;
		demand[result[2]] = d;
	}
	
	private int[] findMinCost() {
		// Find first cost with supply and demand.
		int[] result = {cost[0][0], 0, 0};
		for(int i = 0; i < cost.length; i++){
			for(int j = 0; j < cost[i].length; j++){
				// Exclude costs without supply or demand.
				if (demand[j] == 0)
					continue;
				if (supply[i] == 0)
					continue;
				result = new int[]{cost[i][j], i, j};
				break;
			}
		}
		for(int i = 0; i < cost.length; i++){
			for(int j = 0; j < cost[i].length; j++){
				// Exclude costs without supply or demand.
				if (demand[j] == 0)
					continue;
				if (supply[i] == 0)
					continue;
				
				if(cost[i][j] < result[0]) {
					result[0] = cost[i][j];
					result[1] = i;
					result[2] = j;
				}
			}
		}
		return result;
	}
	
	private int minSupplied(int[] a, int[] b) {
		if (supplied[a[0]][a[1]] < supplied[b[0]][b[1]])
			return supplied[a[0]][a[1]];
		return supplied[b[0]][b[1]];
	}
}