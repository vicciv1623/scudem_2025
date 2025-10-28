#include <iostream>
#include <utility>
#include <cmath>
#include <algorithm>
#include <fstream>
#include <random>
#include <numbers>
#include <array>

using namespace std;

//these are most commonly used parameters
namespace params{
    ofstream fileV;
    ofstream fileP;
    ofstream fileM;
    int airDensity=1;
    int dustDensity=2e6;       //2000000g/m^3
    float alpha=0.5;
    float beta=1.0;
    float radius=1e-6;       //1 micrometer
    int temp;                //kelvin
    int altitude=100;        //100 meters
    float terminalVelocity;
    
    unsigned seed=time(0);
    default_random_engine generator(seed);
}

//this indicates coordinates for position of particle
struct pos3{
    float x;
    float y;
    float z;

    pos3 operator+(const pos3& other) const{
        return {x + other.x, y + other.y, z + other.z};
    }
    pos3 operator-(const pos3& other) const{
        return {x - other.x, y - other.y, z - other.z};
    }
    pos3 operator*(float s) const{
        return {x*s, y*s, z*s};
    }
    void factor(float f){
        float sum = sqrt(pow(x,2) + pow(y,2) + pow(z,2));
        x*=f/sum;
        y*=f/sum;
        z*=f/sum;
    }
    void print(int time){
        params::fileP<<time<<","<<x<<","<<y<<","<<z<<endl;
    }
};

float dragCoeff(){
    return 0.5;
}
//will figure out radius once i have input from johny
void radius(float mass){
    float volume = mass/params::dustDensity;
    params::radius = cbrt(0.75 * volume / M_PI);
}
float surfaceArea(float radius){
    return 4 * M_PI * pow(radius, 2);
}
float humidity(){
    return 1.0;
}
void altitude(pos3 p){
    params::altitude -= p.z;
}
float diffusionCoeff(){
    return 1.0;
}

//dvdt is m/s
float dvdt(float v, float alpha, float beta){
    return -alpha * (0.5 * params::airDensity * pow(v, 2) * dragCoeff() * 
        surfaceArea(params::radius) - beta*humidity());
}

//normal_distribution(mean, stdv)
pos3 drdt(float diffusionCoeff, float velocity){
    normal_distribution<float> dist(0, 2*diffusionCoeff);
    pos3 r;
    r.x = dist(params::generator);
    r.y = dist(params::generator);
    r.z = abs(dist(params::generator));

    r.factor(velocity);

    //cout<<r.x<<" "<<r.y<<" "<<r.z<<endl;
    return r;
}

//edit this as so to take in function as parameter
void adamsBashforth(pair<float, float>& velocity, pair<pos3, pos3> position, pair<float, float> mass, float step, int tStart, int n){
    for(int i=1; i<n; i++){
        float t = tStart + step*i;

        //velocity
        float updateV = velocity.second + 
            step*0.5 * (3*dvdt(velocity.second, params::alpha, params::beta) - 
            dvdt(velocity.first, params::alpha, params::beta));
        
        velocity.first = velocity.second;
        velocity.second = updateV;

        params::fileV<<t<<","<<updateV<<endl;

        //position
        pos3 updateP = position.second + 
            (drdt(2, velocity.second)*3 - drdt(2, velocity.first)) *step*0.5;

        position.first = position.second;
        position.second = updateP;

        params::fileP<<t<<","<<updateP.x<<","<<updateP.y<<","<<updateP.z<<endl;

        //mass
        params::fileM<<t<<","<<mass.first<<endl;

        //miscellaneous
        radius(mass.second);
        cout<<params::radius<<endl;
        altitude(position.second);


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
    pair<float, float> velocity;
    velocity.first = 50;
    velocity.second = velocity.first + step/2*(3*dvdt(velocity.first, 1.0, 1.0));

    //position
    pair<pos3, pos3> position;
    position.first = {0,0,0};
    position.second = {10,10,10};

    //mass
    pair<float, float> mass;
    mass.first = 8.4e-12;
    mass.second = 8.4e-12;

    //file output
    params::fileV.open("results/velocity.txt");
    params::fileV<<"time,velocity"<<endl;
    params::fileV<<tStart<<","<<velocity.first<<endl;

    params::fileP.open("results/position.txt");
    params::fileP<<"time,x,y,z"<<endl;
    position.first.print(tStart);

    params::fileM.open("results/mass.txt");
    params::fileM<<"time,mass"<<endl;
    params::fileM<<tStart<<","<<mass.first<<endl;
    
    tStart+=step;

    params::fileV<<tStart<<","<<velocity.second<<endl;
    position.second.print(tStart);
    params::fileM<<tStart<<","<<mass.second<<endl;
 
    //running simulation...
    adamsBashforth(velocity, position, mass, step, tStart, n);
    params::fileV.close();
    params::fileP.close();
    params::fileM.close();

    return 0;
}