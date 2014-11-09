attribute highp vec2 vertex;
attribute highp vec2 st;
uniform highp mat4 mvp;
varying mediump vec2 tex_st;

void main(void)
{
  gl_Position = mvp * vec4(vertex, 0, 1);
  tex_st = st;
}
