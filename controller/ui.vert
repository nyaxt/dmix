attribute highp vec4 vertex;
attribute highp vec2 st;
uniform highp mat4 mvp;
varying mediump vec2 tex_st;

void main(void)
{
  gl_Position = mvp * vertex;
  tex_st = st;
}
