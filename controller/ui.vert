attribute highp vec4 vertex;
attribute highp vec2 st;
varying mediump vec2 tex_st;

void main(void)
{
  gl_Position = vertex;
  tex_st = st;
}
