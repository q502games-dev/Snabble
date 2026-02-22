export const COLS = 16, ROWS = 12, CELL = 46, GW = COLS * CELL, GH = ROWS * CELL;
export const VOWELS = new Set("AEIOU"), STICKY_SET = new Set("QZXJ");
export const LW = "EEEEEEEEEEEETTTTTTTTTAAAAAAAAOOOOOOOIIIIIIINNNNNNNSSSSSSSHHHHHHRRRRRRLLLLDDDDCCCCUUUMMMMWWFFGGYYPBBVVKKJJXXQZZ";
export const SP = {A:1,B:3,C:3,D:2,E:1,F:4,G:2,H:4,I:1,J:8,K:5,L:1,M:3,N:1,O:1,P:3,Q:10,R:1,S:1,T:1,U:1,V:4,W:4,X:8,Y:4,Z:10};
export const DIR = {UP:{x:0,y:-1},DOWN:{x:0,y:1},LEFT:{x:-1,y:0},RIGHT:{x:1,y:0}};
export const DIRS_ARR = [DIR.UP, DIR.DOWN, DIR.LEFT, DIR.RIGHT];
export const LEVELS = [
  {target:300,spd:270,walls:0,pads:1,portals:0},
  {target:650,spd:260,walls:0,pads:2,portals:0},
  {target:1100,spd:250,walls:1,pads:2,portals:0},
  {target:1600,spd:240,walls:1,pads:2,portals:1},
  {target:2200,spd:230,walls:2,pads:3,portals:1},
  {target:3000,spd:220,walls:2,pads:3,portals:2},
  {target:4000,spd:210,walls:3,pads:4,portals:2},
  {target:5200,spd:200,walls:3,pads:4,portals:2},
  {target:6500,spd:190,walls:4,pads:4,portals:3},
  {target:8000,spd:175,walls:5,pads:5,portals:3},
  {target:10000,spd:160,walls:6,pads:5,portals:3},
  {target:12500,spd:145,walls:8,pads:5,portals:4},
];
export const WORD_TIMER = 10;
export const LEVEL_DURATION = 300;
export const AI_START_LEVEL = 7;
export const FALLING_START_LEVEL = 8;
export const BOOST_INTERVAL = 25000;
export const BOOST_DURATION = 3000;
export const BOOST_SPEED_MULT = 0.4;
export const PITFALL_START_LEVEL = 4;
export const PITFALL_SPAWN_INTERVAL = 6000;
export const PITFALL_MORPH_TIME = 4000;
export const PITFALL_TYPES = ["speed","bomb","letter"];
export const FALLING_SPAWN_INTERVAL = 3500;
export const FALLING_SPEED = 700;
export const PORTAL_REPOSITION_INTERVAL = 12000;
export const EVENT_TYPES = ["wildcard"];
export const EVENT_DUR = {wildcard:20000};
