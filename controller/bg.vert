attribute highp vec2 vertex;

void main(void)
{
  gl_Position = vec4(vertex, 0.5, 1.0);
}
