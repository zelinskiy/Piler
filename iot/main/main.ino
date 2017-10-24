#include <Stepper.h>

const int stepsPerRevolution = 300;  
const int times = 6;
const int buttonPin = 2;
const int silentPin = 3;
const int soundPin = 4;
const int redPin = 5;
const int yellowPin = 6;

bool silent = false;

Stepper myStepper(stepsPerRevolution, 8, 9, 10, 11);

void setup() {
  myStepper.setSpeed(60);
  Serial.begin(9600);
  pinMode(soundPin, OUTPUT);
  pinMode(buttonPin, INPUT);
  digitalWrite(buttonPin, HIGH);
  pinMode(redPin, OUTPUT);
  pinMode(yellowPin, OUTPUT);
  pinMode(silentPin, INPUT);
  digitalWrite(silentPin, HIGH);  
}

void loop() {
  if(digitalRead(buttonPin) == LOW){
    dispence();
  }
  if(digitalRead(silentPin) == LOW){
    mute();
  }
  else if (Serial.available() > 0) {
        int incomingByte = Serial.read();
        Serial.print("I received: ");
        Serial.println(incomingByte, DEC);
        switch(incomingByte){
          case 'F':
            forward();
            break;
          case 'B':
            backward();
            break;
          case 'D':
            dispence();
            break;
          case 'S':
            sound(200);
            break;
          case 'L':
            light(200);
            break;
          case 'N':
            delay(200);
            break;
          case 'M':
            silent = true;
            break;
          case 'W':
            silent = false;
            break;          

        }
    }
}

inline void dispence(){
    if(!silent){
      for(int i = 0; i < 3; i++) sound(50);
    }
    for(int i = 0; i < 4; i++) light(50);
    setLights(0,1);
    forward();
    setLights(1,0);
    backward();
    setLights(0,0);
    for(int i = 0; i < 3; i++) light(50);
    if(!silent){
      for(int i = 0; i < 3; i++) sound(50);
    }
}

inline void mute(){ 
  silent = !silent; 
  setLights(!silent, silent);
  delay(1000);
  setLights(0,0);
}

inline void setLights(int r, int y){
  if (r == 0) digitalWrite(redPin, LOW);
  else digitalWrite(redPin, HIGH);
  if (y == 0) digitalWrite(yellowPin, LOW);
  else digitalWrite(yellowPin, HIGH);
}

inline void forward(){
  Serial.println("forward");
  for(int i = 0; i < times; i++){    
    myStepper.step(stepsPerRevolution);     
  }
}

inline void backward(){
  Serial.println("backward");
  for(int i = 0; i < times; i++){    
    myStepper.step(-stepsPerRevolution);     
  }
}

inline void light(int duration){
  setLights(0,1);
  delay(duration);
  setLights(1,0);
  delay(duration);
  setLights(0,0);
}

inline void sound(int duration){
    digitalWrite(soundPin, HIGH);
    delay(duration);
    digitalWrite(soundPin, LOW);
    delay(duration);
}
