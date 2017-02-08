// Dijkstra.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <iostream>
#include <vector>
#include <set>
#include <list>
#include <chrono>

using namespace std;
using Clock = std::chrono::steady_clock;
using std::chrono::time_point;
using std::chrono::duration_cast;
using std::chrono::milliseconds;
using namespace std::literals::chrono_literals;


struct point {
	int destination;
	double distance;
	point(int _dest, double _distance) : destination(_dest), distance(_distance) {}
};


void dijkstra(int beginPoint, const vector<vector<point>> &graph, vector<double>&minDist, vector<int>&previous) {
	//int graphSize = graph.size();
	minDist.clear();
	minDist.resize(graph.size(), numeric_limits<double>::max());
	minDist[beginPoint] = 0;
	previous.resize(graph.size(), -1);
	set<pair<int, double>> queue;
	queue.insert(std::make_pair(minDist[beginPoint], beginPoint));
	while (!queue.empty())
	{
		double dist = queue.begin()->first;
		int u = queue.begin()->second;
		queue.erase(queue.begin());

		// visit all points connecting to u
		const vector<point> &neighbours = graph[u];
		for (std::vector<point>::const_iterator neighbor_iter = neighbours.begin();
		neighbor_iter != neighbours.end();
			neighbor_iter++)
		{
			int v = neighbor_iter->destination;
			double weight = neighbor_iter->distance;
			double distance_through_u = dist + weight;
			if (distance_through_u < minDist[v]) {
				queue.erase(std::make_pair(minDist[v], v));

				minDist[v] = distance_through_u;
				previous[v] = u;
				queue.insert(std::make_pair(minDist[v], v));

			}

		}
	}
}

list<int> getShortestPath(int vertex, const std::vector<int> &previous)
{
	list<int> path;
	for (; vertex != -1; vertex = previous[vertex])
		path.push_front(vertex);
	return path;
}

int main()
{
	time_point<Clock> start = Clock::now();
	//a = 0, b = 1, c = 2, d = 3, e = 4, f = 5, g = 6, h = 7, i = 8, j = 9, k = 10, l = 11, m = 12, n = 13, z = 15
	vector<vector<point>> graph(14);
	//a
	graph[0].push_back(point(1, 2));
	graph[0].push_back(point(7, 2));
	graph[0].push_back(point(10, 2));
	graph[0].push_back(point(4, 2));
	//b
	graph[1].push_back(point(0, 2));
	graph[1].push_back(point(2, 4));
	graph[1].push_back(point(7, 1));
	graph[1].push_back(point(8, 2));
	//c
	graph[2].push_back(point(1, 4));
	graph[2].push_back(point(3, 3));
	graph[2].push_back(point(8, 2));
	graph[2].push_back(point(9, 4));
	//d
	graph[3].push_back(point(2, 3));
	graph[3].push_back(point(9, 2));
	graph[3].push_back(point(13, 1));
	//e
	graph[4].push_back(point(0, 2));
	graph[4].push_back(point(5, 3));
	graph[4].push_back(point(10, 2));
	//f
	graph[5].push_back(point(4, 3));
	graph[5].push_back(point(6, 4));
	graph[5].push_back(point(10, 2));
	graph[5].push_back(point(11, 1));
	graph[5].push_back(point(12, 3));
	//g
	graph[6].push_back(point(5, 4));
	graph[6].push_back(point(12, 1));
	graph[6].push_back(point(13, 1));
	//h
	graph[7].push_back(point(0, 2));
	graph[7].push_back(point(1, 1));
	graph[7].push_back(point(8, 4));
	graph[7].push_back(point(10, 2));
	//i
	graph[8].push_back(point(1, 2));
	graph[8].push_back(point(2, 2));
	graph[8].push_back(point(7, 4));
	graph[8].push_back(point(9, 5));
	graph[8].push_back(point(10, 3));
	graph[8].push_back(point(11, 1));
	//j
	graph[9].push_back(point(2, 4));
	graph[9].push_back(point(3, 2));
	graph[9].push_back(point(8, 5));
	graph[9].push_back(point(11, 2));
	graph[9].push_back(point(12, 2));
	graph[9].push_back(point(13, 1));
	//k
	graph[10].push_back(point(0, 2));
	graph[10].push_back(point(4, 3));
	graph[10].push_back(point(5, 2));
	graph[10].push_back(point(7, 2));
	graph[10].push_back(point(8, 3));
	graph[10].push_back(point(11, 4));
	//l
	graph[11].push_back(point(5, 1));
	graph[11].push_back(point(8, 1));
	graph[11].push_back(point(9, 2));
	graph[11].push_back(point(10, 4));
	graph[11].push_back(point(12, 3));
	//m
	graph[12].push_back(point(5, 3));
	graph[12].push_back(point(6, 1));
	graph[12].push_back(point(9, 2));
	graph[12].push_back(point(11, 3));
	graph[12].push_back(point(13, 3));
	//n
	graph[13].push_back(point(3, 1));
	graph[13].push_back(point(6, 1));
	graph[13].push_back(point(9, 1));
	graph[13].push_back(point(12, 3));

	vector<double> minDist;
	vector<int> previous;
	char letterpt[] = { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n' };

	dijkstra(0, graph, minDist, previous);
	list<int> path = getShortestPath(13, previous);
	time_point<Clock> end = Clock::now();
	milliseconds diff = duration_cast<milliseconds>(end - start);
	cout << "Distance from a to n: " << minDist[13] << " path: ";
	for each (int pt in path)
	{
		cout << letterpt[pt] << " ";
	}
	cout << endl << "time elapsed: " << diff.count() << "ms" << std::endl;
	getchar();
    return 0;
}

