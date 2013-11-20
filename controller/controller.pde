float global_t;
float[] wavept_y = new float[2];

void setup()
{
  global_t = 0;
  size(800, 480);
  frameRate(60);
  wavept_y[0] = -200;
  wavept_y[1] = 300;
}

void window(int x, int y, int w, int h)
{
  color linec = color(0, 0, 0xcc); //color(0xcc,0xcc,0xff);
  stroke(linec);
  noFill();

  strokeWeight(1);
  rect(x+5, y+5, w-10, h-10);
  line(x+10,y+5, x+5, y+10);
  line(
      x+w-10, y+h-5,
      x+w-5, y+h-10);
  line(
      x+w-15, y+5,
      x+w-5, y+15);
  line(
      x+5, y+h-15,
      x+15, y+h-5);
}
void wavebg()
{
  stroke(0x80, 0x80, 0xfc);
  noFill();
  strokeWeight(3);
  for(int i = 0; i < 2; ++ i)
    wavept_y[i] = sin(global_t/(100-20*i)) * 200;
  for(int i = 0; i < 10; ++ i)
  {
    stroke(0xe0, 0xe0, 0xff, 256.0 / 10 * i);

    int y = 250-i;
    bezier(
        -100, y,
        800/3, y+wavept_y[0],
        800*2/3, y+wavept_y[1],
        900, y);
  }
}

void draw()
{
  background(0xec,0xec,0xec);
  /*
  wavebg();
  global_t += 1;
  
  fill(0,0,0);
  text(int(frameRate), 30, 30);
  */

  for(int i = 0; i < 4; ++ i)
  {
    window(200*i, 0, 200, 480);
  }
}

