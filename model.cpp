#include <iostream>
#include <utility>
#include <cmath>
#include <algorithm>
#include <fstream>

using namespace std;

//put these in a namespace
ofstream file;
int airDensity=1; //will upate later with actual values
int dustDensity=2; // 2g / cm^3

//radius will be in cm
//time step will be in second
//temperature will be in Kelvin
//current altitude will be in ... 

//will update the three functions 
float dragCoeff(){
    return 1.0;
}
float radius(){
    return 1.0;
}
float surfaceArea(){
    return 1.0;
}
float humidity(){
    return 1.0;
}
float altitude(){
    return 1.0;
}

float dvdt(float v, float alpha, float beta){
    return -alpha * (0.5 * airDensity * pow(v, 2) * dragCoeff() * surfaceArea() - beta*humidity());
}

void adamsBashforth(pair<float, float>& p, float step, int tStart, int n){
    for(int i=1; i<n; i++){
        float t = tStart + step*i;
        float updateP = p.second + step/2 * (3*dxdt(p.second) - dxdt(p.first));
        
        p.first = p.second;
        p.second = updateP;

        file<<t<<","<<p.second<<endl;
    }
}

void particle(){
    //combine all functions above in this particle function
}

int main(){
    int n = 60;
    int tStart = 2000;
    int tEnd = 2020;
    float step = (tEnd-tStart)/float(n);

    //velocity
    pair<float, float> p;
    p.first = 6;
    p.second = p.first + step/2*(3*dxdt(p.first));

    file.open("results.txt");
    file<<"time,value"<<endl;
    file<<tStart<<","<<p.first<<endl;
    tStart+=step;
    file<<tStart<<","<<p.second<<endl;

    adamsBashforth(p, step, tStart, n);
    file.close();

    return 0;
}