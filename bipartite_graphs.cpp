// Emily S. Rose
// Wheaton College Class of 2015
// Computer Science Senior Seminar
// February 2015
//
// CONTENTS:
// bipartite_graphs.cpp contains the main driver code for creating a graph on the console.  The program provides the user with the options of creating their own graph or importing one from a file.  The user is then able to add and delete edges between nodes, check for the existance of an edge, display the entire graph to the console, or display the graph by only showing the existing edges between nodes. 
//

#include "stdafx.h"

#include <fstream>
#include <iostream>
#include <string>

#include "adj_matrix.h"

//Global variables
string userInput;	//will hold the user's input for each prompt
string filename;	//will hold the filename of the graph the user would like to import
string line;	//will hold one line at a time as they are read in from file
int nodeCount;	//the number of nodes to be created for graph
int commaLoc;	//holds the index location of the comma between the two nodes
int from;		//holds the node # of the edge's starting node
int to;			//holds the node # of the edge's ending node
bool direct;	//whether or not the graph will be directed

using namespace std;
void menu(adj_matrix myGraph);
void createFromScratch();
void importFromFile();

//main driver function
void main(){
	//Display primary menu to console
	cout << "Please select one of the following: " << endl;
	cout << "   (A) Create graph from scratch" << endl;
	cout << "   (B) Import graph from file" << endl;


	//Get selection from user
	cin >> userInput;
	cout << endl << endl << endl;

	//Change input to uppercase
	userInput = toupper(userInput[0]);

	//Create a graph from scratch
	if(userInput == "A")
		createFromScratch();
	else if (userInput == "B")
		importFromFile();
	else{
		cout << "I'm sorry, I don't understand your selection." << endl;
		main();
	}
}

//Function is called if user selects to create a graph from scratch
//INPUTS: 
//	instructions entered from the command prompt by the user
//OUTPUTS:
//	prompts are printed to the screen, walking the user through each step of the process
//	adj_matrix is created with the parameters set by the user
void createFromScratch(){
	//Display header
	cout << "Creating a graph from scratch" << endl;
	cout << "-----------------------------" << endl;

	//Display prompt for user to provide a nodeCount
	cout << "If you wish to display your graph on the console at any point, you will need to use 19 nodes or fewer.  How many nodes would you like your graph to have?" << endl;
	cin >> userInput;
	cout << endl;

	nodeCount = atoi(userInput.c_str());	//Change string to int and assign value to nodeCount variable.

	//Display prompt for whether user wants the graph to be directed or not
	cout << "Would you like this graph to be directed? (Y/N)" << endl;
	cin >> userInput;
	cout << endl;

	//Change input to uppercase
	userInput = toupper(userInput[0]);

	//Set 'direct' value to true or false, determined by userInput
	if(userInput == "Y")
		direct = true;
	else
		direct = false;

	//Create adjacency matrix with given values
	adj_matrix myGraph(nodeCount, direct);

	//title the graph
	cout << "What would you like to title the graph?" << endl;
	cin >> myGraph.title;

	//Continue to the main menu
	menu(myGraph);
}

//Function is called if user selects to import a graph from a file
//INPUTS:
//	REQUIRED IMPORT FILE FORMAT: 
//	Any file to be imported into this program as graph data must have the following format:
//		- The first line must include ONLY the number of nodes in the graph
//		- The second line must include either a 1 (Yes) or a 0 (No), representing whether or not the user requires the graph to be directed.
//		- The remaining lines will each include an ordered pair of nodes, separated by a comma and then a space, representing where an edge exists. For a directed graph, ( ex. 4, 5) means that an edge will be created coming out of node 4 and going into node 5.
//	User is also prompted to add a title to the graph.
//OUTPUTS:
//	prompts are printed to the screen, walking the user through each step of the process
//	adj_matrix is created with the parameters set by the file
void importFromFile(){
	//Display header
	cout << "Importing graph from file" << endl;
	cout << "---------------------------" << endl;

	//Display prompt for user to provide filename
	cout << "Please enter the file name or path to the file: ";
	cin >> filename;
	cout << endl;

	//Open filestream
	fstream fin;
	int i = 0;
	fin.open(filename);
	if (fin.is_open()){
		while ((!fin.eof()) && i < 2){
			getline(fin, line);

			//If this is the first line of the file, it contains the nodeCount
			if (i == 0)
				nodeCount = atoi(line.c_str());
			//This is the second line, which contains a 0(no) or 1(yes), depending on whether or not the user wants the graph to be directed.
			else{
				if (atoi(line.c_str()) == 1)
					direct = true;
				else
					direct = false;
			} 
			i++;
		}

		//Create adjacency matrix with given values
		adj_matrix myGraph(nodeCount, direct);

		//Read in the rest of the file, which contains node pairs representing edges
		while (!fin.eof()){
			getline(fin, line);

			//Skip blank lines
			if(line == ""){}
			else{
				commaLoc = line.find(",");
				//Convert node #'s from string to int, skipping the comma and space between the node #'s
				from = atoi((line.substr(0, commaLoc)).c_str());
				to = atoi((line.substr(commaLoc+2, line.length())).c_str());

				//Add edge to the graph
				myGraph.addEdge(from, to);
			}
			i++;
		}
		cout << "Read successful." << endl;
		fin.close();

		//title the graph
		cout << "What would you like to title the graph?" << endl;
		cin >> myGraph.title;

		//Continue to the main menu
		menu(myGraph);
	}

	else{
		cout << "Cannot open file. Try again." << endl;
		importFromFile();
	}
}

//Provides menu to console, accepts user input, and calls the appropriate functions based on user input
//INPUTS:
//	User inputs to console responses based on the prompts
//OUTPUTS:
//	Prompts printed to console providing user with directions
//	functions are called based on user input
void menu(adj_matrix myGraph){
	//Create space before menu
	cout << endl << endl << endl;

	//Display main menu
	cout << "Please choose one of the following: " << endl;
	cout << "1. Create edge" << endl;
	cout << "2. Remove edge" << endl;
	cout << "3. Determine if there is an edge between two nodes" << endl;
	cout << "4. Display full graph" << endl;
	cout << "5. Display graph (edges only)" << endl;
	cout << "6. Check if graph is bipartite" << endl;
	cout << "0. Quit" << endl;
	cin >> userInput;
	cout << endl;

	//If the user picked 1, 2, or 3, get starting and ending nodes
	if ((userInput == "1") || (userInput == "2") || (userInput == "3")){
		cout << "Please enter the number associated with the node at the start of the edge: ";
		cin >> from;
		cout << endl;
		//from = atoi(from.c_str());

		cout << "Please enter the number associated with the node at the other end of the edge: ";
		cin >> to;
		cout << endl;
		//to = atoi(to.c_str());
	}

	//If the user wishes to add an edge to the graph, call .addEdge()
	if (userInput == "1")
		myGraph.addEdge(from, to);

	//If the user wishes to remove an edge to the graph, call .removeEdge()
	else if (userInput == "2")
		myGraph.removeEdge(from, to);

	//If the user wishes to add check if an edge is in the graph, call .checkEdge()
	else if (userInput == "3"){
		if (myGraph.checkEdge(from, to))
			cout << "Edge Exists." << endl;
		else
			cout << "No Edge Found." << endl;
	}

	//If the user wishes to display the full graph
	else if (userInput == "4"){
		cout << myGraph.title << endl;
		cout << "-------------------" << endl;
		myGraph.print(true);	//true: we do want zeros displayed
		system("pause");
	}

	//If the user wishes to display only the edges & nodes in the graph
	else if (userInput == "5"){
		cout << myGraph.title << endl;
		cout << "-------------------" << endl;
		myGraph.print(false);		//false: we don't want zeros
		system("pause");
	}
	else if (userInput == "6"){
		if (myGraph.is_bipartite())
			cout << "This graph is bipartite." << endl;
		else
			cout << "This graph is NOT bipartite." << endl;

	}

	//If the user wishes to end
	else if (userInput == "0"){
		myGraph.~myGraph();
		return;
	}

	//Recall main menu so user can perform next task
	menu(myGraph);

}
