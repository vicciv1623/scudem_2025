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
    ofstream fileO;

    const int dustDensity=2e3;       //2000 kg/m^3
    const float alpha=0.5;
    const float beta=1.0;
    const float terminalVelocity=70;

    float radius=1e-6;       //1 micrometer
    float temp;                //kelvin
    float altitude=100;        //100 meters
    float airViscosity;
    float diffusionCoeff;
    float airDensity;
    
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
float surfaceArea(){
    //surface area of a sphere
    return 4 * M_PI * pow(params::radius, 2);
}
void upRadius(float mass){
    //volume of a sphere
    float volume = mass/params::dustDensity;
    params::radius = cbrt(0.75 * volume / M_PI);
}
void upAltitude(pos3 p){
    params::altitude -= p.z;
}
void upAirViscosity(){
    //Sutherland's law
    params::airViscosity = 1.716e-5 * pow(params::temp/273, 1.5) * 384 / (params::temp + 111);
}
void upDiffusionCoeff(){
    //Stoke-Einstein equation
    params::diffusionCoeff = 1.38e-23 * params::temp / (6 * M_PI * params::airViscosity * params::radius);
}
//void upTemp(){}
void upAirDensity(){
    //https://www.grc.nasa.gov/www/k-12/airplane/atmosmet.html
    //https://www.eoas.ubc.ca/courses/atsc113/flying/met_concepts/02-met_concepts/02a-std_atmos-P/index.html

    //trophosphere
    float pressure;
    if(params::altitude < 11000){
        pressure = 101.29 * pow(params::temp/288.08, 5.256);
    }
    //lower stratosphere
    else if(params::altitude < 25000){
        pressure = 22.65 * exp(1.73 - 0.000157*params::altitude);
    }
    //upper stratosphere
    else{
        pressure = 2.488 / pow(params::temp/216.6, 11.388);
        cout<<pressure<<endl;
    }

    params::airDensity = pressure / (0.2869 * params::temp);
}

//differential equations
//dvdt is m/s
float dvdt(float v, float alpha, float beta, float mass){
    return -(alpha * 0.5 * params::airDensity * pow(v, 2) * dragCoeff() * surfaceArea() + mass*9.81);
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

float dmdt(float mass){
    return 3*mass / params::radius; //add drdt??
}

void adamsBashforth(pair<float, float>& velocity, pair<pos3, pos3> position, pair<float, float> mass, int t){
    //velocity
    float updateV = velocity.second + 
        0.5 * (3*dvdt(velocity.second, params::alpha, params::beta, mass.second) - 
        dvdt(velocity.first, params::alpha, params::beta, mass.second));
    
    velocity.first = velocity.second;
    velocity.second = updateV;

    params::fileV<<t<<","<<updateV<<endl;

    //position
    pos3 updateP = position.second + 
        (drdt(2, velocity.second)*3 - drdt(2, velocity.first)) *0.5;

    position.first = position.second;
    position.second = updateP;

    params::fileP<<t<<","<<updateP.x<<","<<updateP.y<<","<<updateP.z<<endl;

    //mass
    params::fileM<<t<<","<<mass.first<<endl;

    //miscellaneous
    upRadius(mass.second);
    upAltitude(position.second);
    //upTemp
    upAirViscosity();
    upDiffusionCoeff();
    upAirDensity();
}

void particle(){
    //combine all functions above in this particle function
    gamma_distribution<float> initVel(15, 0.9);     //will scale this *1000
    normal_distribution<float> initMass(0.00001, 0.000003);

    //initialize values
    pair<float, float> velocity;
    pair<pos3, pos3> position;
    pair<float, float> mass;

    velocity.first = abs(initVel(params::generator));
    position.first = {0,0,0};
    mass.first = abs(initMass(params::generator));

    upRadius(mass.first);

    params::fileV<<1<<","<<velocity.first<<endl;   
    position.first.print(1);
    params::fileM<<1<<","<<mass.first<<endl;

    velocity.second = velocity.first + 0.5*(3*dvdt(velocity.first, 1.0, 1.0, mass.first));
    position.second = {10, 10,10};   //fix this
    mass.second = 1.7e-14;          //fix this

    params::fileV<<2<<","<<velocity.first<<endl;   
    position.first.print(2);
    params::fileM<<2<<","<<mass.first<<endl;

    upRadius(mass.second);
    upAltitude(position.second);

    int t=3;
    while(params::altitude > 0){
        adamsBashforth(velocity, position, mass, t);
        t++;
    }
}   

int main(){
    params::fileV.open("results/velocity.txt");
    params::fileV<<"time,velocity"<<endl;

    params::fileP.open("results/position.txt");
    params::fileP<<"time,x,y,z"<<endl;

    params::fileM.open("results/mass.txt");
    params::fileM<<"time,mass"<<endl;
    
    params::fileO.open("results/other.txt");
    params::fileO<<"time,radius,temp,alt,air_visc,diff_coeff,air_dens"<<endl;

    for(int i=0; i<1; i++){
        particle();
    }

    params::fileV.close();
    params::fileP.close();
    params::fileM.close();
    params::fileO.close();

    return 0;
}