// Emily Rose
// Let's Make Some Change

#include "stdafx.h"
#include <iostream>
using namespace std;

const int DENOMINATIONS[] = { 1, 7, 30, 84, 235 };

class changeStruct{
public:
	int desiredAmount;
	int coinCount;
	int count[5];

	changeStruct(){
		desiredAmount = 0;
		coinCount = 0;
		count[0] = 0;
		count[1] = 0;
		count[2] = 0;
		count[3] = 0;
		count[4] = 0;

	};

	changeStruct min(changeStruct temp){
		if (this->coinCount < temp.coinCount)
			return *this;
		else return temp;
	}
};

changeStruct ChangeMaking(int desiredChange, int size, int F[], int coinsUsed[])
{
	int max = 5;
	if (desiredChange < 5)
		max = desiredChange;


	F[0] = 0;
	for (int j = 1; j <= desiredChange; j++) {
		F[j] = desiredChange;
		for (int i = 0; i < size; i++) {
			if (j >= DENOMINATIONS[i] && 1 + F[j - DENOMINATIONS[i]] < F[j]) {
				F[j] = 1 + F[j - DENOMINATIONS[i]];

				coinsUsed[j] = i;
			}
		}
	}

	changeStruct A;


	int temp;
	int j = desiredChange;
	while (j){
		temp = DENOMINATIONS[coinsUsed[j]];
		for (int i = 0; i < max; i++){
			if (temp == DENOMINATIONS[i])
				A.count[i]++;
		}
		j = j - DENOMINATIONS[coinsUsed[j]];
	}

	A.coinCount = F[desiredChange];

	cout << "Minimum Coin Count: " << A.coinCount << endl;
	cout << "Coins Used: " << endl;
	cout << "1's: " << A.count[0]  << endl;
	cout << "7's: " << A.count[1]  << endl;
	cout << "30's: " << A.count[2]  << endl;
	cout << "84's: " << A.count[3]  << endl;
	cout << "235's: " << A.count[4]  << endl;

	
	return A;
}

void main()
{
	int desiredChange = 0;

	while (desiredChange >= 0){
		cout << endl << "Please enter the amount of change you would like: ";
		cin >> desiredChange;
		cout  << endl;
		if (desiredChange >= 0)	//Do nothing for negative numbers
		{
			if (desiredChange <= 6){		//exceptions thrown if desiredChange<=6, so they are handled here.
				cout << "Minimum Coin Count: " << desiredChange << endl;
				cout << "Coins Used: " << endl;
				cout << "1's: " << desiredChange << endl;
				cout << "7's: " << 0 << endl;
				cout << "30's: " << 0 << endl;
				cout << "84's: " << 0 << endl;
				cout << "235's: " << 0 << endl;
			}
			else{
				int size = sizeof(DENOMINATIONS) / sizeof(DENOMINATIONS[0]);
				int *F = new int[desiredChange + 1];
				int *coinsUsed = new int[desiredChange + 1];
				changeStruct A = ChangeMaking(desiredChange, size, F, coinsUsed);

				//system("PAUSE");
				delete[] F;
				delete[] coinsUsed;
			}
		}
	}



	cout << "Thanks for playing!" << endl;

}
