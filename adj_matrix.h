//PROGRAMMER: EMILY S. ROSE
//SPRING 2015

#ifndef ADJ_MATRIX_H
#define ADJ_MATRIX_H

#pragma once
#include <iostream>
#include <fstream>
#include <string>
#include <queue>

using namespace std;

class adj_matrix
{
public:
	// Constructor: accepts POSITIVE integer representing the number of nodes in the graph
	adj_matrix(int n, bool d);

	~adj_matrix();

	void addEdge(int from, int to);
	void removeEdge(int from, int to);
	bool checkEdge(int from, int to);
	void print(bool zeros);
	bool is_bipartite();
	string title_;

private:

	// add edge helper function
	void addEdge_h(int from, int to);

	// remove edge helper function
	void removeEdge_h(int from, int to);

	// check edge helper function
	bool checkEdge_h(int from, int to);

	// display graph
	void print_h(bool zeros);

	bool is_bipartite_h(int start);
	
	int** matrix_;
	int nodeCount_;
	bool directed_;
};

#endif  //ADJ_MATRIX_H
